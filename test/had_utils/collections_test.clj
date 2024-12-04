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
