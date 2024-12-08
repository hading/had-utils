(ns had-utils.collections-test
  (:require [clojure.test :refer :all]
            [had-utils.collections :as hc]))

(def small-grid [[1]])
(def medium-grid [[1 2 3] [4 5 6] [7 8 9]])
(def non-square-grid [[1 2 3] [4 5 6]])

(deftest test-border
  (testing "Can add a border to a grid"
    (is (= [[0 0 0] [0 1 0] [0 0 0]] (hc/border small-grid 0)))))

(deftest test-border-and-flatten
  (testing "Can add a border to a grid and flatten it"
    (is (= [0 0 0 0 1 0 0 0 0] (hc/border-and-flatten small-grid 0)))))

(deftest test-borderv
  (let [bordered-grid (hc/borderv small-grid 0)
        more-bordered-grid (hc/borderv small-grid 0 2)]
    (testing "Can add a border to a grid and ensure it is vectors"
      (is (= [[0 0 0] [0 1 0] [0 0 0]] bordered-grid))
      (is (vector? bordered-grid))
      (is (every? vector? bordered-grid)))
    (testing "Can add a multi-char border to a grid and ensure it is vectors"
      (is (= [[0 0 0 0 0]
              [0 0 0 0 0]
              [0 0 1 0 0]
              [0 0 0 0 0]
              [0 0 0 0 0]]) more-bordered-grid))))

(deftest test-bracket
  (testing "Can bracket collections with a value"
    (is (= [2 2] (hc/bracket [] 2)))
    (is (= [9 1 2 3 9] (hc/bracket [1 2 3] 9)))))

(deftest test-bracket
  (let [b1 (hc/bracketv (list) 2)
        b2 (hc/bracketv (range 3) 9)]
    (testing "Can bracket collections with a value and return vectors."
      (is (= [2 2] b1))
      (is (vector? b1))
      (is (= [9 0 1 2 9] b2))
      (is (vector? b2)))))

(deftest test-count-by
  (testing "We can count numbers of elements of a collection with given function values"
    (is (= {0 3 1 4 2 4} (hc/count-by #(mod % 3) [34 29 38 48 2 0 4 4 2 9 1])))))

(deftest test-count-when
  (let [coll [0 1 2 3 4]]
      (testing "Count with trivial predicate"
        (is (= 5 (hc/count-when (constantly true) coll))))
      (testing "Count with non-trivial predicate"
        (is (= 3 (hc/count-when even? coll)))
        (is (= 2 (hc/count-when odd? coll))))))

(deftest test-filter-first
  (testing "We can find the first element of a sequence satisfying a predicate"
    (is (= 0 (hc/filter-first (partial < -1) (range))))
    (is (= 5 (hc/filter-first (partial < 4.5) (range))))
    (is (= nil (hc/filter-first (constantly true) [])))
    (is (= nil (hc/filter-first (partial < 100) (range 10))))))

(deftest test-filter-grid-coordinates
  (testing "We can get points in a grid where the value satisifes a predicate"
    (is (= [] (hc/filter-grid-coordinates (partial < 10) medium-grid)))
    (is (= [[0 1] [1 0] [1 2] [2 1]] (hc/filter-grid-coordinates even? medium-grid)))))

(deftest test-filter-in-grid
  (testing "We can filter a collection of points to determine which are in a grid"
    (is (= [[0 0] [2 2]]
           (hc/filter-in-grid medium-grid [[0 0] [-1 1] [1 3] [2 2]])))))

(deftest test-grid-centered-segment
  (testing "We can get a segment in a grid centered at a particular point"
    (is (= [2 5 8] (hc/grid-centered-segment medium-grid 1 [1 1] [1 0])))
    (is (= [7 5 3] (hc/grid-centered-segment medium-grid 1 [1 1] [-1 1])))))

(deftest test-grid-coordinates
  (testing "We can get all points in a grid"
    (is (= [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]] (hc/grid-coordinates medium-grid))))
  (testing "We can get all points in a grid padded a certain distance from the edge"
    (is (= [[1 1]] (hc/grid-coordinates medium-grid 1))))
  (testing "We can get all points in a grid padded differently for rows and columns."
    (is (= [[1 0] [1 1] [1 2]] (hc/grid-coordinates medium-grid 1 0)))))

(deftest test-grid-flat-map
  (testing "We can get a sequence from mapping a function over a grid"
    (is (= [1 2 3 0 1 2 3 0 1] (hc/grid-flat-map (fn [g p] (mod (get-in g p) 4)) medium-grid))))
  (testing "We can get a sequence from mapping a function over a grid with padding."
    (is (= [2 1 0] (hc/grid-flat-map (fn [g p] (mod (get-in g p) 4)) medium-grid 0 1)))))

(deftest test-grid-mapcat
  (testing "We can map a function producing a sequence over a grid and concat the results"
    (is (= [1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9]
           (hc/grid-mapcat (fn [g p] (let [x (get-in g p)] [x x])) medium-grid)))))

(deftest test-grid-segment
  (testing "We can extract segments from grids"
    (is (= [1 2 3] (hc/grid-segment medium-grid 3 [0 0] [0 1])))
    (is (= [1 4 7] (hc/grid-segment medium-grid 3 [0 0] [1 0])))
    (is (= [9 5] (hc/grid-segment medium-grid 2 [2 2] [-1 -1]))))
  (testing "We can extract segments from grids and transform the result with a function"
    (is (= 12 (hc/grid-segment medium-grid 3 [0 0] [1 0] (partial apply +))))))

(deftest test-grid-simple-map
  (testing "We can map a function over a grid"
    (is (= [[2 3 4] [5 6 7]]) (hc/grid-simple-map inc medium-grid))))

;;I'm not going to do this one now.
(deftest test-grid-to-graph)

(deftest test-in-grid-pred
  (testing "We can get a predicate that says if a point is in a grid"
    (let [p (hc/in-grid-pred medium-grid)]
      (is (p [0 0]))
      (is (p [2 2]))
      (is (not (p [-1 1])))
      (is [not (p [1 3])]))))

(deftest test-in-grid?
  (testing "We can check if a point is in a grid"
    (is (hc/in-grid? medium-grid [0 0]))
    (is (hc/in-grid? medium-grid [2 2]))
    (is (not (hc/in-grid? medium-grid [-1 1])))
    (is [not (hc/in-grid? medium-grid [1 3])]))
  (testing "We can check if a point is in a grid specifying row and column separately"
    (is (hc/in-grid? medium-grid 2 2))
    (is (not (hc/in-grid? medium-grid -1 1)))))

(deftest test-map-kv
  (let [coll {1 2 3 4 5 6}]
    (testing "Test val-fn only"
      (is (= {1 4 3 16 5 36} (hc/map-kv (fn [k v] (* v v)) coll))))
    (testing "Replicate invert-map"
      (is (= {2 1 4 3 6 5} (hc/map-kv (fn [k v] v) (fn [k v] k) coll))))
    (testing "More complicated map-kv"
      (is (= {3 2 7 12 11 30} (hc/map-kv (fn [k v] (+ k v)) (fn [k v] (* k v)) coll))))))

(deftest test-neighbors-2d
  (testing "We can get the coordinates of neighboring points in a grid, not going outside the grid, optionally with diagonals."
    (is (= [[0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]]
           (hc/neighbors-2d medium-grid [1 1] {:with-diagonal true})))
    (is (= [[0 1] [1 0] [1 2] [2 1]]
           (hc/neighbors-2d medium-grid [1 1])))
    (is (= [[0 0] [0 2] [1 0] [1 1] [1 2]] (hc/neighbors-2d medium-grid [0 1] {:with-diagonal true})))
    (is (= [[0 1] [1 0] (hc/neighbors-2d medium-grid [0 0])]))))

(deftest test-neighbors-2d-map
  (testing "We can get a map of neighbors in a grid to their values, optionally with diagonals."
    (is (= {[0 0] 1 [0 2] 3 [1 1] 5} (hc/neighbors-2d-map medium-grid [0 1])))
    (is (= {[0 0] 1 [0 2] 3 [1 0] 4 [1 1] 5 [1 2] 6} (hc/neighbors-2d-map medium-grid [0 1] {:with-diagonal true})))))

(deftest test-neighbors-2d-map
  (testing "We can get a seq of values of neighbors in a grid, optionally with diagonals."
    (is (= [1 3 5] (hc/neighbors-2d-vals medium-grid [0 1])))
    (is (= [1 3 4 5 6] (hc/neighbors-2d-vals medium-grid [0 1] {:with-diagonal true})))))

(deftest test-pairs
  (testing "We can get all pairs in a sequence"
    (is (empty? (hc/pairs [])))
    (is (empty? (hc/pairs [1])))
    (is (= [[0 1] [0 2] [1 2]]) (hc/pairs (range 3)))))

(deftest test-subgrid
  (testing "We can get a subgrid of a grid"
    (is (= [[1 2] [4 5]] (hc/subgrid medium-grid 0 2 0 2)))
    (is (= [[2 3] [5 6] [8 9]] (hc/subgrid medium-grid 0 3 1 3)))))

(deftest test-transpose
  (testing "We can transpose a grid"
    (is (= small-grid (hc/transpose small-grid)))
    (is (= [[1 4 7] [2 5 8] [3 6 9]] (hc/transpose medium-grid)))
    (is (= [[1 4] [2 5] [3 6]] (hc/transpose non-square-grid)))))


(deftest test-transposev
  (testing "We can transpose a grid, ensuring the result is a vector of vectors"
    (let [grid '((1 2 3) (4 5 6))
          transposed-grid (hc/transposev grid)]
      (is (= [[1 4] [2 5] [3 6]] transposed-grid))
      (is (vector? transposed-grid))
      (is (every? vector? transposed-grid)))))
