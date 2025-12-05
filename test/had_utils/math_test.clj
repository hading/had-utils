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

(deftest put-into-range
  (testing "We can find which number is a range is equivalent to a number mod the size of the range."
    (is (= 1 (hm/put-into-range 0 9 1)))
    (is (= 1 (hm/put-into-range 0 9 101)))
    (is (= 1 (hm/put-into-range 0 9 -99)))
    (is (= 20 (hm/put-into-range 12 27 100)))
    (is (= 19 (hm/put-into-range 12 27 -541)))))

(deftest test-mod-op
  (testing "We can apply an operation to two numbers and move the result into a given range"
    (is (= 1 (hm/mod-op 1 10 + 0 1)))
    (is (= 1 (hm/mod-op 1 10 + 100 1)))
    (is (= 3 (hm/mod-op 0 9 - 0 7)))))

(deftest test-digits->num
  (testing "We can convert digits to a number."
    (is (= 3 (hm/digits->num [3])))
    (is (= 5132 (hm/digits->num [5 1 3 2])))
    (is (= 10 (hm/digits->num [1 0 1 0] 2)))
    (is (= 0 (hm/digits->num [])))))

(deftest test-num->digits
  (testing "We can convert a number to digits."
    (is (= [3] (hm/num->digits 3)))
    (is (= [5 1 3 2] (hm/num->digits 5132)))
    (is (= [1 0 1 0] (hm/num->digits 10 2)))
    (is (= [] (hm/num->digits 0)))))

(deftest test-range-count
  (testing "We can count the number of integers in an integral range."
    (is (= 1 (hm/range-count [0 0])))
    (is (= 2 (hm/range-count [3 4])))
    (is (= 11 (hm/range-count [10 20])))
    (is (= 11 (hm/range-count [20 10])))))

(deftest test-combine-ranges
  (testing "We can combine a sequence of ranges into a sequence of non-overlapping
ranges covering the same numbers"
    (is (= [] (hm/combine-ranges [])))
    (is (= [[1 2]] (hm/combine-ranges [[1 2]])))
    (is (= [[1 10]] (hm/combine-ranges (for [x (range 1 10)] [x (inc x)]))))
    (is (= [[1 10]] (hm/combine-ranges (reverse (for [x (range 1 10)] [x (inc x)])))))
    (is (= [[1 10]] (hm/combine-ranges (shuffle (for [x (range 1 10)] [x (inc x)])))))
    (is (= [[1 10]] (hm/combine-ranges [[1 10] [2 3] [4 8] [9 10]])))
    (is (= [[1 4]] (hm/combine-ranges [[1 3] [2 4]])))
    (is (= [[1 2] [3 4]]) (hm/combine-ranges [[1 2] [3 4]]))
    (is (= [[1 2] [3 6]]) (hm/combine-ranges [[4 6] [1 2] [3 5]]))
    (is (= [[1.0 3.4] [3.5 6.4] [7.0 8.0]]
           (hm/combine-ranges [[1.0 2.0] [1.5 3.0] [1.2 3.4] [3.5 4.5] [4.0 4.1] [4.4 6.4] [7.0 8.0]])))))
