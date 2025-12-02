(ns had-utils.string
  (:require
   [clojure.string :as str]))

(defn split*
  "Equivalents for the standard string/split function with the first two arguments reversed."
  ([re s] (str/split s re))
  ([re s limit] (str/split s re limit)))
