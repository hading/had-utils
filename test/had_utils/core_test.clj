(ns had-utils.core-test
  (:require [clojure.test :refer :all]
            [had-utils.core :refer :all]))

(deftest test-map-map
  (let [coll {1 2 3 4 5 6}]
    (testing "Test val-fn only"
      (is (= {1 4 3 16 5 36} (map-map coll (fn [k v ] (* v v))))))
    (testing "Replicate invert-map"
      (is (= {2 1 4 3 6 5} (map-map coll (fn [k v] v) (fn [k v] k)))))
    (testing "More complicated map-map"
      (is (= {3 2 7 12 11 30} (map-map coll (fn [k v] (+ k v)) (fn [k v] (* k v))))))))

(deftest test-count-when
  (let [coll [0 1 2 3 4]]
      (testing "Count with trivial predicate"
        (is (= 5 (count-when (constantly true) coll))))
      (testing "Count with non-trivial predicate"
        (is (= 3 (count-when even? coll)))
        (is (= 2 (count-when odd? coll))))))


