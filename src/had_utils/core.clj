(ns had-utils.core)

;;This should become irrelevant when Clojure 1.11 adds this type of function
(defn parse-int
  "Parse integer with default radix 10."
  ([str]
   (Integer/parseInt str))
  ([str radix]
   (Integer/parseInt str radix)))

(defn abs [x]
  "Absolute value of the number x"
  (if (< x 0) (- x ) x))

(defn iterate-until [f x pred]
  "Iterate one argument function f on starting value x
and return the first iterate that makes pred true."
  (->> (iterate f x)
       (filter pred)
       first))
