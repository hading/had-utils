(ns had-utils.collections
  (:require
   [ubergraph.core :as uc]
   [had-utils.math :as hm]))

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

(defn filter-first
  "Find the first element in `coll` for which `pred` returns true"
  [pred coll]
  (first (filter pred coll)))

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

(def straight-2d-offsets
  "Rectilinear offsets in 2d."
  (vec (sort [[1 0] [-1 0] [0 1] [0 -1]])))
(def diagonal-2d-offsets
  "Diagonal offsets in 2d."
  [[-1 -1] [1 -1] [-1 1] [1 1]])
(def all-2d-offsets
  "Rectilinear and diagonal offsets in 2d"
  (vec (sort (concat straight-2d-offsets diagonal-2d-offsets))))

(defn in-grid?
  "Is the given row and column in the grid?"
  ([grid [row col]] (in-grid? grid row col))
  ([grid row col] (and (< -1 row (count grid))
                       (< -1 col (count (first grid))))))

(defn in-grid-pred
  "Returns a predicate on row and col or [row col] that says if
  that row and column are in the grid."
  [grid]
  (fn
    pred
    ([[row col]] (pred row col))
    ([row col] (and (< -1 row (count grid))
                    (< -1 col (count (first grid)))))))

(defn neighbors-2d
  "`loc` is a [row col] coordinate in a 2d grid `grid`. Gives a vector of coordinates of horizontal
  and vertical neighbors, and also diagonal ones if `:with-diagonal` is true. Does not give neighbors
  that exceed the bounds of the grid."
  [grid loc & {:keys [:with-diagonal]}]
  (let [offsets (if with-diagonal all-2d-offsets straight-2d-offsets)]
    (->> offsets
         (mapv (partial hm/add-vectors loc))
         (filterv (in-grid-pred grid)))))

(defn neighbors-2d-map
    "`loc` is a [row col] coordinate in a 2d grid `grid`. Gives a map of coordinates of horizontal
  and vertical neighbors, and also diagonal ones if `:with-diagonal` is true, to their values in the grid.
  Does not give neighbors that exceed the bounds of the grid."
  [grid loc & {:keys [:with-diagonal]}]
  (reduce (fn [acc loc] (assoc acc loc (get-in grid loc)))
          {}
          (neighbors-2d grid loc :with-diagonal with-diagonal)))

(defn neighbors-2d-vals
    "`loc` is a [row col] coordinate in a 2d grid `grid`. Gives a seq of values of horizontal
  and vertical neighbors, and also diagonal ones if `:with-diagonal` is true, to their values in the grid.
  Does not give neighbors that exceed the bounds of the grid."
  [grid loc & {:keys [:with-diagonal]}]
  (vals (neighbors-2d-map grid loc :with-diagonal with-diagonal)))

(defn grid-to-graph
  "Makes an ubergraph graph from the grid and edge-fn. If directed is true it is a directed graph.
  For each location [row col] we call edge-fn with the grid, the location, and each neighbor of the
  location (including diagonal neighbors if :with-diagonal is true). If edge-fn is true an edge is
  created from location to neighbor. If it is a number then that is assigned as the weight of the edge.
  Note that for undirected graphs edge-fn should be symmetrical in the location and neighbor location
  or there may be unexpected behavior, as it will be called twice."
  [grid edge-fn & {:keys [:with-diagonal :directed]}]
  (let [graph (if directed (uc/digraph) (uc/graph))
        update-fn (fn [g [location neighbor]]
                    (let [result (edge-fn grid location neighbor)]
                      (cond
                        (number? result) (uc/add-edges g [location neighbor result])
                        result (uc/add-edges g [location neighbor])
                        :otherwise g)))]
    (reduce update-fn graph (concat (for [row (range (count grid))
                                          col (range (count (first grid)))
                                          n (neighbors-2d grid [row col] :with-diagonal with-diagonal)]
                                      [[row col] n])))))

(defn pairs
  "Given `seq` (x0 x1 ... xn) returns a sequence of pairs
  ((x0 x1) (x0 x2) ... (x0 xn) (x1 x2) ...)"
  [seq]
  (loop [seq seq
         pairs []]
    (let [ys (rest seq)]
      (if (empty? ys)
        pairs
        (recur ys
               (concat pairs (map (partial list (first seq)) ys)))))))
