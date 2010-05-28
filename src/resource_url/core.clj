(ns resource-url.core
  (:import (java.net URLEncoder))
  (:require
     [clojure.contrib.string :as s]
     [clojure.contrib.except :as exc]))


(defn url-encode-str [s] (URLEncoder/encode (s/as-str s) "UTF-8"))

(defn url-encode-map [m]
  (s/join "&"
          (map
            (fn [[k v]]
              (str (url-encode-str k) "=" (url-encode-str v)))
            m)))

(defn url [parts & objs]
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

(defmacro named-url [name & parts]
  `(defn ~name [& args#]
     (apply url (vector ~@parts) args#)))

(defmacro polymorphic-url [& args]
  `(defmulti ~@args))

(defn- def-url-method [multi dispatch f]
  `(defmethod ~multi ~dispatch [obj# & args#]
     (let [args# (if (vector? obj#) (concat obj# args#) (cons obj# args#))]
       (apply ~f args#))))

(defmacro resource-url [poly-function & pairs]
  `(do
     ~@(map #(apply def-url-method poly-function %) (partition 2 pairs))))


(defn dispatch [f]
  (fn [obj & args]
    (f
       (if (vector? obj)
         (last obj)
         obj))))

(def  by-type (dispatch type))
(defn by-member [mem] (dispatch mem))
