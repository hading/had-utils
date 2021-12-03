(ns had-utils.core)

;;This should become irrelevant when Clojure 1.11 adds this type of function
(defn parse-int
  "Parse integer with default radix 10."
  ([str]
   (Integer/parseInt str))
  ([str radix]
   (Integer/parseInt str radix)))
