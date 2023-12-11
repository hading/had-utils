(ns had-utils.math)

(defn gcd
  "Find the greatest common denominator of `a` and `b`"
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Find the least common multiple of `a` and `b`"
  [a b]
  (/ (* a b) (gcd a b)))

(defn l1-distance
  "Find the l1 distance between points `p1` and `p2`"
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (apply +)))

(defn linf-distance
  "Find the l-infinity distance between points `p1` and `p2`"
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (apply max)))
