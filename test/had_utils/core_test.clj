(ns had-utils.core-test
  (:require [clojure.test :refer :all]
            [had-utils.core :as hc]
            [had-utils.io :as hio]
            [had-utils.collections :as hcoll]))

(deftest test-map-kv
  (let [coll {1 2 3 4 5 6}]
    (testing "Test val-fn only"
      (is (= {1 4 3 16 5 36} (hcoll/map-kv (fn [k v] (* v v)) coll))))
    (testing "Replicate invert-map"
      (is (= {2 1 4 3 6 5} (hcoll/map-kv (fn [k v] v) (fn [k v] k) coll))))
    (testing "More complicated map-map"
      (is (= {3 2 7 12 11 30} (hcoll/map-kv (fn [k v] (+ k v)) (fn [k v] (* k v)) coll))))))

