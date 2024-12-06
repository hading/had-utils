(ns had-utils.math)

(defn gcd
  "Find the greatest common denominator of `a` and `b`.
   Always return a non-negative number."
  [a b]
  (loop [a (abs a)
         b (abs b)]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm
  "Find the least common multiple of `a` and `b`, both positive."
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

(defn add-vectors
  "Add two vectors of numbers of the same dimension"
  [v1 v2]
  (mapv + v1 v2))
