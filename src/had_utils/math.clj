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
  "Add two or more vectors of numbers of the same dimension"
  [v1 & vs]
  (reduce (partial mapv +) v1 vs))

(defn invert-vector
  "Multiply vector by -1"
  [v]
  (mapv (partial -) v))

(defn subtract-vectors
  "Subtract two vectors of numbers of the same dimension"
  [v1 v2]
  (add-vectors v1 (invert-vector v2)))

(defn scale-vector
  "Scale the vector `v` by a factor of `x`"
  [v x]
  (map (partial * x) v))

(defn average-vectors
  "Average vectors `v1` and `v2`. If `w` is supplied then
  make a weighted average where `v1` has weight `w` and
  `v2` weight `1-w`. One can also think of this as the midpoint
  of two points."
  ([v1 v2] (average-vectors v1 v2 1/2))
  ([v1 v2 w] (add-vectors (scale-vector v1 w)
                          (scale-vector v2 (- 1 w)))))

(defn ray
  "Starting at and including `point` a collection of points
  moving in steps given by `dir`."
  [point dir]
  (iterate (partial add-vectors dir) point))

(defn ray-segment
  "the first `n` points on the ray defined by `point` and `dir`"
  [n point dir]
  (take n (ray point dir)))
