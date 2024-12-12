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
    (is (= [9 9 7 6] (hm/add-vectors v3 v4)))
    (is (= [10 7] (hm/add-vectors v1 v1 v2)))
    (is (= v3 (hm/add-vectors v3)))))

(deftest test-average-vectors
  (testing "We can find the average of two vectors"
    (is (= [2 3] (hm/average-vectors [1 2] [3 4]))))
  (testing "We can find the weighted average of two vectors."
    (is (= [1 2] (hm/average-vectors [1 2] [3 4] 1)))
    (is (= [3 4] (hm/average-vectors [1 2] [3 4] 0)))
    (is (= [2 3] (hm/average-vectors [1 2] [3 4] 1/2)))
    (is (= [3 3] (hm/average-vectors [0 0] [9 9] 2/3)))))

(deftest test-gcd
  (testing "GCD of numbers"
    (is (= 101 (hm/gcd 0 101)))
    (is (= 1 (hm/gcd 13 97)))
    (is (= 7 (hm/gcd 35 77))))
  (testing "GCD of negatives gives a positive"
    (is (= 1 (hm/gcd 13 -97)))
    (is (= 1 (hm/gcd -13 97)))
    (is (= 7 (hm/gcd -35 -77)))))

(deftest test-invert-vector
  (testing "Inverting vector"
    (is (= [0 0] (hm/invert-vector [0 0])))
    (is (= [-1 -2 -3] (hm/invert-vector [1 2 3])))))

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

(deftest test-scale-vector
  (testing "We can scale vectors"
    (is (= [0 0] (hm/scale-vector [1 2] 0)))
    (is (= [1 2] (hm/scale-vector [1 2] 1)))
    (is (= [2 4] (hm/scale-vector [1 2] 2)))
    (is (= [-1 -2] (hm/scale-vector [1 2] -1)))))

(deftest test-subtract-vectors
  (testing "We can subtract vectors"
    (is (= [0 0] (hm/subtract-vectors v1 v1)))
    (is (= [7 1] (hm/subtract-vectors v2 v1)))))
