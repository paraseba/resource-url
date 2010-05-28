(ns resource-url.core
  "Create functions to generate URL paths.

   Main function is named-url, used to define new functions. You can create
   polymorphic functions using polymorphic-url and resource-url."
  (:import (java.net URLEncoder))
  (:require
     [clojure.contrib.def :as deff]
     [clojure.contrib.string :as s]
     [clojure.contrib.except :as exc]))


(defn url-encode-str
  "Encode a string according to the application/x-www-form-urlencoded MIME format

   Calls clojure.contrib.string/as-str on it's argument"
  [s]
  (URLEncoder/encode (s/as-str s) "UTF-8"))

(defn url-encode-map
  "Encodes a map of parameters in a format valid for URLs.

   Calls url-encode-map on all keys and values, and separates all name/value pairs
   with a '&' character"
  [m]
  (s/join "&"
          (map
            (fn [[k v]]
              (str (url-encode-str k) "=" (url-encode-str v)))
            m)))

(defn url
  "Generate a URL based on a template and substitution objects.

   parts is collection of strings and functions, objs will be used in order as
   argument for each function part.

   Extra parameters can be given as name/value pairs, and an special :anchor value
   can be passed too.
  
   Users don't normally call this function"
  [parts & objs]
  (let [[ev-parts params] (reduce
                            (fn [[out vals] part]
                              (if (string? part)
                                [(conj out part) vals]
                                (if (first vals)
                                  [(conj out (part (first vals))) (next vals)]
                                  (exc/throw-arg "Can't generate url for parts %s from %d objects" parts (count objs)))))
                            [[] objs]
                            parts)

        params (apply hash-map params)
        anchor (url-encode-str (:anchor params))
        params (dissoc params :anchor)
        base (map #(s/replace-first-re #"/+$" "" (s/as-str %)) ev-parts)
        base (s/join "/" base)
        base (if (empty? params)
               base
               (str base "/?" (url-encode-map params)))]

    (if (s/blank? anchor)
      base
      (str base "#" anchor))))

(defmacro named-url
  "Defines a function with the given name which returns a URL according to
   a template and after doing substitutions and encoding.

   parts is a series of strings and functions with define the URL template.
   When the defined function is called, each parts string will be generated
   in the URL as is. Each function will be called on the arguments passed.

   Extra parameters can be given as name/value pairs, and an special :anchor value
   can be passed too

   Example:

     (named-url user-path \"users\" :id)

     (user-path {:id 20} :lang :en :anchor :profile)

     will generate /users/20/?lang=en#profile

   Notice that in the call to named-url :id is just a function (since keywors
   can be used this way), you could use your own functions instead.

   You can also generate nested URLs adding more parts and passing several
   objets in a vector:

     (named-url picture-path \"users\" :url-slug \"pictures\" :id)

     (picture-path [{:url-slug \"john-doe\"} {:id 1}] :lang :en :anchor :profile)

     will generate /users/john-doe/pictures/1/?lang=en#profile"
  [name & parts]
  `(defn ~name [& args#]
     (apply url (vector ~@parts) args#)))

(defmacro polymorphic-url
  "Defines a polymorphic URL generation function.

   Use in combination with resource-url. Receives the same arguments that
   clojure.core/defmulti"
  [& args]
  `(defmulti ~@args))

(defn- def-url-method [multi dispatch f]
  `(defmethod ~multi ~dispatch [obj# & args#]
     (let [args# (if (vector? obj#) (concat obj# args#) (cons obj# args#))]
       (apply ~f args#))))

(defmacro resource-url
  "Maps a resource type to a URL generating function.

   f is the name of a polymorphic function created with
   polymorphic-url. args is a sequence of dispatch-value/url-function.
   Url-functions should be created using named-url.

   Example:

   (polymorphic-url url-for type) ; this creates url-for polymorphic function
                                  ; url-for will dispatch to url generating functions
                                  ; based on type of it's argument

   ; we create our \"types\"
   (defrecord User [id])
   (defrecord Post [id])

   ; we define URL templates for each type
   (named-url user-path \"users\" :id)

   ; posts are nested inside their owners
   (named-url post-path \"users\" :id \"posts\" :id)

   ; we map each type to it's URL generating function
   (resource-url url-for
     Post post-path
     User user-path)

   Then you could do

   (url-for (User. 1)) ; will generate /users/1
   (url-for [(User. 1) (Post. 5)] :page 1) ; will generate /users/1/posts/5/?page=1"
  [f & args]
  `(do
     ~@(map #(apply def-url-method f %) (partition 2 args))))


(defn dispatch
  "Helper to create dispatcher functions for polymorphic-url.

   f is a function that receives the last mapped object and returns the dispatch value."
  [f]
  (fn [obj & args]
    (f
       (if (vector? obj)
         (last obj)
         obj))))

(deff/defvar by-type (dispatch type)
  "Dispatcher by clojure.core/type for polymorphic-url

   Dispatches on the type of the object passed, or the type of the last element
   if the object is a vector")

(defn by-member
  "Helper function to dispatch polymarphic URL generation functions based on
   \"members\" of the object. Use in combination with polymorphic-url.

   For instance given (def user {:role :admin, :id 5, :name \"John\")
   you could use (by-member :role)"
  [mem] (dispatch mem))
