(ns had-utils.math)

(defn gcd
  "Find the greatest common denominator of `a` and `b`"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  "Find the least common multiple of `a` and `b`"
  (/ (* a b) (gcd a b)))
