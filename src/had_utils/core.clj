(ns had-utils.core
  (:require [clojure.string :as str]))

(defn slurp-split-map
  "Slurp a file, split into parts on a regexp,
  and map a function over the result."
  [path split-re map-fn]
  (map map-fn (-> (slurp path)
                  (str/split split-re))))

(defn slurp-split
  "Slurp a file and split into parts on a regexp."
  [path split-re]
  (-> (slurp path)
      (str/split split-re)))

(defn slurp-lines-map
  "Slurp a file, split into lines, and map a function over the result."
  [path map-fn]
  (slurp-split-map path #"\n" map-fn))

(defn slurp-lines
  "Slurp a file and split into lines"
  [path]
  (str/split-lines (slurp path)))

(defn map-map
  "Construct a new map from an existing one.
  Each of val-fn and (optional - default (fn [k v] k)) key-fn are
  a function of two arguments, the key and value.
  Note that to make the usage more natural the optional key-fn
  argument goes in the second place when used."
  ([map val-fn]
   (map-map map val-fn (fn [k v] k)))
  ([map key-fn val-fn]
   (reduce-kv #(assoc %1 (key-fn %2 %3) (val-fn %2 %3)) {} map)))

