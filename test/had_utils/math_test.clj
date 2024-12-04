(ns had-utils.math-test
  (:require [clojure.test :refer :all]
            [had-utils.math :as hm]))

(def v1 [1 2])
(def v2 [8 3])
(def v3 [1 2 3 4])
(def v4 [8 7 4 2])

(deftest test-add-vectors
  (testing "Add vectors"
    (is (= [9 5] (hm/add-vectors v1 v2)))
    (is (= [9 9 7 6] (hm/add-vectors v3 v4)))))

(deftest test-gcd
  (testing "GCD of numbers"
    (is (= 101 (hm/gcd 0 101)))
    (is (= 1 (hm/gcd 13 97)))
    (is (= 7 (hm/gcd 35 77))))
  (testing "GCD of negatives gives a positive"
    (is (= 1 (hm/gcd 13 -97)))
    (is (= 1 (hm/gcd -13 97)))
    (is (= 7 (hm/gcd -35 -77)))))

(deftest test-lcm
  (testing "LCM of numbers"
    (is (= 77 (hm/lcm 11 7)))
    (is (= 60 (hm/lcm 12 20)))))

(deftest test-l1-distance
  (testing "l1 distance between vectors"
    (is (= 8 (hm/l1-distance v1 v2)))
    (is (= 15 (hm/l1-distance v3 v4)))
    (is (= 0 (hm/l1-distance v1 v1)))))

(deftest test-linf-distance
  (testing "l infinity distance between vectors"
    (is (= 7 (hm/linf-distance v1 v2)))
    (is (= 7 (hm/linf-distance v3 v4)))
    (is (= 0 (hm/linf-distance v3 v3)))))
