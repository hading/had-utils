(ns had-utils.io
  (:require [clojure.string :as str]))

(defn slurp-split-map
  "Slurp the file at `path`, split into parts on `split-re`,
  and map `map-fn` over the result."
  [path split-re map-fn]
  (map map-fn (-> (slurp path)
                  (str/split split-re))))

(defn slurp-split
  "Slurp the file at `path` and split into parts on `split-re`."
  [path split-re]
  (slurp-split-map path split-re identity))

(defn slurp-lines-map
  "Slurp the file at `path`, split into lines, and map `map-fn` over the result."
  [path map-fn]
  (slurp-split-map path #"\n" map-fn))

(defn slurp-grid
  "Slurp the file at `path`, making a two-d grid of one character substrings. If
  `f` is supplied then map it over each substring."
  ([path] (slurp-grid path identity))
  ([path f]
   (-> (slurp-lines-map path #(mapv f (str/split % #"")))
       vec)))

(defn slurp-lines
  "Slurp the file at `path` and split into lines"
  [path]
  (str/split-lines (slurp path)))

(defn line->ints
  "Get integers from a string. Assumes the string begins and ends with a digit."
  [line]
  (->> (str/split line #"\D+")
       (mapv parse-long)))
