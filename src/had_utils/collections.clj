(ns had-utils.collections)

(defn map-kv
  "Construct a new map from an existing one.
  Each of `val-fn` and `key-fn` (optional - default `(fn [k _] k))` are
  a function of two arguments, the key and value.
  Note that to make the usage more natural the optional `key-fn`
  is the first argument when used."
  ([val-fn coll]
   (map-kv (fn [k _] k) val-fn coll))
  ([key-fn val-fn coll]
   (reduce-kv #(assoc %1 (key-fn %2 %3) (val-fn %2 %3)) {} coll)))

(defn count-when
  "Count the number of elements in `coll` where `f` returns true.
  If not supplied use identity as `f`."
  ([coll] (count-when coll identity))
  ([coll f] (count (filter f coll))))

(defn count-by
  "Return a map from the distinct values of `f` applied to `coll`
  to the frequencies they occur."
  [f coll]
  (->> (group-by f coll)
       (map-kv (fn [k v] (count v)))))

(defn transpose
  "Transpose a rectangular sequence of sequences."
  [seqs]
  (loop [xs seqs acc []]
    (if (empty? (first xs))
      acc
      (recur (map rest xs) (conj acc (map first xs))))))

(defn transposev
  "Transpose a rectangular sequence of sequences,
  returning a vector of vectors."
  [seqs]
  (->>
   (transpose seqs)
   (map vec)
   vec))

;;;The next few are useful largely for AoC problems
(defn bracket
  "Add the element `i` to the start and end of `coll`."
  [coll i]
  (concat [i] coll [i]))

(defn bracketv
  "Add the element `i` to the start and end of `coll` and return a vector."
  [coll i]
  (vec (bracket coll i)))

(defn border
  "`grid` should be a rectangular collection of collections. Adds `i`
as a border around the supplied grid."
  [grid i]
  (let [bracketed-grid (map #(bracket % i) grid)
        cols (count (first bracketed-grid))
        border-row (take cols (repeat i))]
    (concat [border-row] bracketed-grid [border-row])))

(defn borderv
  "`grid` should be a rectangular collection of collections. Adds `i`
  as a border around `grid`. Return a vector of vectors"
  [grid i]
  (let [bracketed-grid (mapv #(bracketv % i) grid)
        cols (count (first bracketed-grid))
        border-row (vec (take cols (repeat i)))]
    (vec (concat [border-row] bracketed-grid [border-row]))))

(defn border-and-flatten
  "Borders `grid` with `i` and then makes it into a
one dimensional vector"
  [grid i]
  (vec (flatten (border grid i))))

(defn neighbors
  "`loc` is assumed to be a coordinate into a flattened two-dimensional grid with `cols` columens.
  Gives a vector of coordinates of the horizontal and vertical neighbors. Does not check bounds."
  [cols loc]
  (mapv (partial + loc) [-1 1 cols (- cols)]))

(defn neighbors-with-diagonals
    "`loc` is assumed to be a coordinate into a flattened two-dimensional grid with `cols` columens.
  Gives a vector of the coordinates of horizontal, vertical, and diagonal neighbors.
  Does not check bounds."
  [cols loc]
  (let [ncols (- cols)]
    (mapv (partial + loc) [-1 1 cols (inc cols) (dec cols) ncols (inc ncols) (dec ncols)])))
