(ns had-utils.collections-test
  (:require [clojure.test :refer :all]
            [had-utils.collections :as hcoll]))

(deftest test-count-when
  (let [coll [0 1 2 3 4]]
      (testing "Count with trivial predicate"
        (is (= 5 (hcoll/count-when (constantly true) coll))))
      (testing "Count with non-trivial predicate"
        (is (= 3 (hcoll/count-when even? coll)))
        (is (= 2 (hcoll/count-when odd? coll))))))

(deftest test-map-kv
  (let [coll {1 2 3 4 5 6}]
    (testing "Test val-fn only"
      (is (= {1 4 3 16 5 36} (hcoll/map-kv (fn [k v] (* v v)) coll))))
    (testing "Replicate invert-map"
      (is (= {2 1 4 3 6 5} (hcoll/map-kv (fn [k v] v) (fn [k v] k) coll))))
    (testing "More complicated map-kv"
      (is (= {3 2 7 12 11 30} (hcoll/map-kv (fn [k v] (+ k v)) (fn [k v] (* k v)) coll))))))
