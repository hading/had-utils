(ns had-utils.io
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
