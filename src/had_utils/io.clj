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
  (-> (slurp path)
      (str/split split-re)))

(defn slurp-lines-map
  "Slurp the file at `path`, split into lines, and map `map-fn` over the result."
  [path map-fn]
  (slurp-split-map path #"\n" map-fn))

(defn slurp-lines
  "Slurp the file at `path` and split into lines"
  [path]
  (str/split-lines (slurp path)))

