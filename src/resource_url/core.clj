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

