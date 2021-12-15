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

(defn reverse-args [f]
  "Given a function returns a new function that takes its
arguments in the reverse order."
  (fn [& args] (apply f (reverse args))))

(defn flip-args [f]
  "Given a function that takes at least two arguments
returns a new function that reverses the order of
the first two arguments"
  (fn [x y & rest]
    (apply f y x rest)))
