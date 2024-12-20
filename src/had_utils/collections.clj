(ns had-utils.collections
  (:require
   [ubergraph.core :as uc]
   [ubergraph.alg :as ua]
   [had-utils.math :as hm]
   [clojure.set :as s]))

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
  ([coll] (count-when identity coll))
  ([f coll] (count (filter f coll))))

(defn count-by
  "Return a map from the distinct values of `f` applied to `coll`
  to the frequencies they occur."
  [f coll]
  (->> (group-by f coll)
       (map-kv (fn [k v] (count v)))))

(defn filter-first
  "Find the first element in `coll` for which `pred` returns true.
  nil if none are found."
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
  as a border around `grid`. If `n` >= 1 is supplied do this n times.
  Return a vector of vectors"
  ([grid i n]
   (let [bracketed-grid (mapv #(bracketv % i) grid)
         cols (count (first bracketed-grid))
         border-row (vec (take cols (repeat i)))
         bordered-grid (vec (concat [border-row] bracketed-grid [border-row]))]
     (if (= n 1)
       bordered-grid
       (borderv bordered-grid i (dec n)))))
  ([grid i] (borderv grid i 1)))

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

(defn grid=
  "true iff the value in the `grid` at `p1` equals that at `p2`.
  If `f` is supplied then compare the value of `f` evaluated on the
  values at the points instead of the values at the points themselves."
  ([grid p1 p2] (grid= grid p1 p2 identity))
  ([grid p1 p2 f] (= (f (get-in grid p1))
                     (f (get-in grid p2)))))

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

(defn filter-in-grid
  "Return only those points in the grid"
  [grid points]
  (filter (in-grid-pred grid) points))

(defn grid-value
  "Return the value of the `grid` at `point`"
  [grid point]
  (get-in grid point))

(defn grid-has-value?
  "Return whether the `grid` has value `val` at point `point`."
  [grid val point]
  (= val (grid-value grid point)))

(defn not-grid-has-value?
  "Return whether the `grid` does not have value `val` at point `point`"
  [grid val point]
  (not (grid-has-value? grid val point)))

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


(defn pairs
  "Given `seq` (x0 x1 ... xn) returns a sequence of pairs
  ((x0 x1) (x0 x2) ... (x0 xn) (x1 x2) ...). If `pred`, a
  function of two variables, is  provided then filter the pairs with that predicate."
  ([seq]
   (loop [seq seq
          pairs []]
     (let [ys (rest seq)]
       (if (empty? ys)
         pairs
         (recur ys
                (concat pairs (map (partial list (first seq)) ys)))))))
  ([seq pred]
   (filter (partial apply pred) (pairs seq))))

(defn coordinate-segment
  "Get coordinates in the grid starting at `point` going in `direction`,
  including the `point`. `length` defines the maximum length returned.
  If the grid is exited then only return points in the grid, even if
  that is shorter than `length`."
  [grid length point direction]
  (->> (hm/ray-segment length point direction)
       (filterv (in-grid-pred grid))))

(defn grid-segment
  "In `grid` get `length` entries going in `direction` [dx dy]
   starting at `point` [row col] as a vector. If `f` is supplied
   then call f on the vector instead. Only use coordinates in the grid."
  ([grid length point direction f]
   (->> (coordinate-segment grid length point direction)
        (mapv #(get-in grid %))
        f))
  ([grid length point direction] (grid-segment grid length point direction identity)))

(defn grid-centered-segment
  "In `grid` from `point` get `length` entries on either side (and point) starting at
  point + length - direction and going to point + length * (direction).
  Result returned as a vector. If `f` is supplied then call f on the vector. Only do coordinates
  in the grid.
  E.g. if the grid is [[1 2 3] [4 5 6] [7 8 9]] then (grid-centered-segment grid 1 [1 1] [1 0])
  returns [2 5 8]"
  ([grid length point direction] (grid-centered-segment grid length point direction identity))
  ([grid length point direction f]
   (let [start (hm/add-vectors point (map (partial * (- length)) direction))]
     (grid-segment grid (inc (* 2 length)) start direction f))))

(defn grid-coordinates
  "All indexes of the 2-d `grid` as [row col] vecs. If `padding` is supplied then
  omit that many rows and cols around the edges. If `col-padding` and `row-padding` are
  supplied then omit respectively for rows and cols."
  ([grid] (grid-coordinates grid 0 0))
  ([grid padding] (grid-coordinates grid padding padding))
  ([grid row-padding col-padding]
   (for [row (range row-padding (- (count grid) row-padding))
         col (range col-padding (- (count (first grid)) col-padding))]
     [row col])))

(defn filter-grid-coordinates
  "All indexes of the 2-d `grid` as [row col] vecs where the value satisfies the
  predicate `f`. If `padding` is supplied then
  omit that many rows and cols around the edges. If `col-padding` and `row-padding` are
  supplied then omit respectively for rows and cols."
  ([f grid] (filter-grid-coordinates f grid 0 0))
  ([f grid padding] (filter-grid-coordinates f grid padding padding))
  ([f grid row-padding col-padding]
   (for [row (range row-padding (- (count grid) row-padding))
         col (range col-padding (- (count (first grid)) col-padding))
         :when (f (grid-value grid [row col]))]
     [row col] )))

(defn find-in-grid
  "Find all coordinates in `grid` where the value is `val`"
  [grid val]
  (filter-grid-coordinates (partial = val) grid))

(defn find-first-in-grid
  "Find the first coordinate in `grid` where the value is `val.`"
  [grid val]
  (first (find-in-grid grid val)))

(defn grid-mapcat
  "Map `f`, a function of the grid and point in the grid and returning a collection,
  over `grid` and concatenate the results.
  If `padding`, `row-padding`, `col-padding` are used then restrict the coordinates
  as in `grid-coordinates`."
  ([f grid] (grid-mapcat f grid 0 0))
  ([f grid padding] (grid-mapcat f grid padding padding))
  ([f grid row-padding col-padding]
   (mapcat #(f grid %) (grid-coordinates grid row-padding col-padding))))

(defn grid-flat-map
  "Map `f`, a function of the grid and point in the grid, over `grid` and resulting in a flat sequence, so
  the structure of the grid is lost.
  If `padding`, `row-padding`, `col-padding` are used then restrict the coordinates as in `grid-coordinates."
  ([f grid] (grid-flat-map f grid 0 0))
  ([f grid padding] (grid-flat-map f grid padding padding))
  ([f grid row-padding col-padding]
   (map #(f grid %) (grid-coordinates grid row-padding col-padding))))

(defn grid-map
    "Map `f`, a function of the grid and point in the grid, over `grid` and resulting in a new grid.
  If `padding`, `row-padding`, `col-padding` are used then restrict the coordinates as in `grid-coordinates`, so
  the new grid will be smaller if there is padding"
  ([f grid] (grid-map f grid 0 0))
  ([f grid padding] (grid-map f grid padding padding))
  ([f grid row-padding col-padding]
   (->> (grid-flat-map f grid row-padding col-padding)
        (partition (- (count (first grid)) (* 2 col-padding)))
        (map vec)
        vec)))

(defn grid-simple-map
  "A convenience to map `f` over `grid`, returning a vector of vectors"
  [f grid]
  (mapv (partial mapv f) grid))

(defn subgrid
  "Get a subgrid of the given grid with the normal start/end conventions.
   Returns a vector of vectors."
  [grid row-start row-end col-start col-end]
  (->>
   (for [row (range row-start row-end)
         col (range col-start col-end)]
     (get-in grid [row col]))
   (partition (- col-end col-start))
   (map vec)
   vec))

(defn grid-to-graph
  "Makes an ubergraph graph from the grid and edge-fn. If directed is true it is a directed graph.
  For each location [row col] we call edge-fn with the grid, the location, and each neighbor of the
  location (including diagonal neighbors if :with-diagonal is true). If edge-fn is true an edge is
  created from location to neighbor. If it is a number then that is assigned as the weight of the edge.
  Note that for undirected graphs edge-fn should be symmetrical in the location and neighbor location
  or there may be unexpected behavior, as it will be called twice. If :all-nodes is true than add nodes
  even if they don't have any edges."
  [grid edge-fn & {:keys [:with-diagonal :directed :all-nodes]}]
  (let [graph (if directed (uc/digraph) (uc/graph))
        update-fn (fn [g [location neighbor]]
                    (let [result (edge-fn grid location neighbor)]
                      (cond
                        (number? result) (uc/add-edges g [location neighbor result])
                        result (uc/add-edges g [location neighbor])
                        :otherwise g)))
        graph-with-edges (reduce
                          update-fn
                          graph
                          (concat (for [row (range (count grid))
                                        col (range (count (first grid)))
                                        n (neighbors-2d grid [row col] :with-diagonal with-diagonal)]
                                    [[row col] n])))]
    (if all-nodes
      (uc/add-nodes* graph-with-edges (grid-coordinates grid))
      graph-with-edges)))

;;; My AOC 2024 day-12 solution has some code related to making a
;;; graph out of a grid, finding the connected components, and then
;;; making a 'boundary graph' from each connected component, where
;;; this is a graph that has nodes that are formed from the nodes of
;;; the component as follows - for each node in the component, and for
;;; each neighbor, if the neighbor is _not_ in the component then
;;; create a 'boundary node', with coordinates between the node and
;;; neighbor, 1/3 (not magic - but one wants this to be < 1/2 to make
;;; sure certain cases are handled properly. And it can be smaller
;;; without any problem.) of the distance to the neighbor. E.g. if [0
;;; 0] is in the component but [0 1] is not then [0 1/3] is created as
;;; a boundary node.  These nodes are then hooked up if they are
;;; adjacent to each other, which is easily tested as both their l1
;;; and linf distances being 1.  The number of connected components of
;;; the boundary graph is equal to the number of sides of the region
;;; defined by the connected component. The perimeter is just the number
;;; of nodes.

(defn gg-unconnected-neighbors
  "For `grid-graph` a set of rectilinear neighbors of `node` in the grid
  not connected to node by an edge."
  [grid-graph node]
  (s/difference (set (map (partial hm/add-vectors node) straight-2d-offsets))
                (set (uc/neighbors grid-graph node))))

(defn gg-node-boundary-points
  "For `grid-graph` a set of 'boundary points' to `node`
  - these are points displaced off of `node` a distance `d` in
  the directions where `node` does not have a neighbor in the graph.
  In order to get the right boundary we do _not_ restrict to
  neighbors in the grid!"
  ([grid-graph node] (gg-node-boundary-points grid-graph node 1/3))
  ([grid-graph node d] (set (map (fn [nb] (hm/average-vectors nb node d))
                                 (gg-unconnected-neighbors grid-graph node)))))

(defn gg-component-boundary-points
  "For `grid-graph` a set of 'boundary points' for the connected component
  `component`."
  [grid-graph component]
  (reduce (fn [acc node]
            (s/union acc (gg-node-boundary-points grid-graph node)))
          #{}
          (set component)))

(defn gg-make-boundary-graph
  "For `grid-graph` a 'boundary graph' for the connected component `component`
  that has boundary nodes as constructed by `gg-component-boundary-points` and
  edges between adjacent points on each side of the connected component's region
  as viewed in the original grid."
  [grid-graph component]
  (let [boundary-points (gg-component-boundary-points grid-graph component)]
    (reduce (fn [acc [p1 p2]]
              (if
                  (= 1 (hm/linf-distance p1 p2) (hm/l1-distance p1 p2))
                (uc/add-edges acc [p1 p2])
                acc))
            (uc/add-nodes* (uc/graph) boundary-points)
            (pairs boundary-points))))

(defn bg-side-count
  "For a grid boundary graph `boundary-graph` for a connected region in a grid-graph
  the number of sides that regions has."
  [boundary-graph]
  (->> boundary-graph
       ua/connected-components
       count))

(defn bg-perimeter
  "For a grid boundary graph `boundary-graph` for a connected region in a grid-graph
  the perimeter of that region"
  [boundary-graph]
  (uc/count-nodes boundary-graph))
