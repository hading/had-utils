(ns had-utils.core-test
  (:require [clojure.test :refer :all]
            [had-utils.core :as hcore]))

;;;used for cycle testing
;;; starting with 5 repeats the value 4 at 3, 6, ... iterations
(defn collatz [n]
  (if (odd? n) (inc (* 3 n)) (/ n 2)))

(deftest test-find-cycle
  (testing "Find a beginning and of cycle"
    (is (= [3 6] (hcore/find-cycle 5 collatz)))))

(deftest test-find-cyclic
  (testing "If there is a cycle we can find the value after a very large number of iterations more efficiently"
    ;;the value after the large number of iterations is the same as that after just a few, so we can do it
    (is (= 2 (hcore/find-cyclic 5 3000000000000000004 collatz)))))

(deftest test-find-cyclic-equivalent
  (testing "Find number of iterations needed to do a large number of iterations when there is a cycle"
    ;;this means that iterating 3000004 times is the same as 4 times
    (is (= 4 (hcore/find-cyclic-equivalent 5 3000004 collatz)))))

(deftest test-fixed-point-n
  (testing "Already at fixed point"
    (is (= [[] 0] (hcore/fixed-point-n (partial drop 1) []))))
  (testing "Non trivial fixed point"
    (is (= [[] 5] (hcore/fixed-point-n (partial drop 1) (range 5)))))
  (testing "Fixed point detection stops if max iterations exceeded."
    (is (= nil (hcore/fixed-point-n (partial drop 1) (range 5) 4))))
  (testing "Fixed point detection can use a test-fn to determine when to stop."
    ;;; So the idea is that the iterating function produces larger and larger collections,
    ;;; but we use a test-fn to chop off the first three, and those are the same after
    ;;; three iterations.
    (is (= [[0 0 0] 3] (hcore/fixed-point-n #(conj % 0) [] nil (partial take 3))))))

(deftest test-flip-args
  (testing "We can flip the first two arguments of a function with two or more arguments."
    (let [f (fn [x y z] (* z (- x y)))]
      (is (= 18 ((hcore/flip-args f) 4 7 6))))))

(deftest test-iterate-until
  (testing "Iterate until a goal is reached"
    (is (= 32 (hcore/iterate-until (partial * 2) 1 (partial < 24))))
    (is (= 7 (hcore/iterate-until inc 1 #(zero? (mod % 7))))))
  (testing "Trivial iteration until goal is reached"
    (is (= 1 (hcore/iterate-until (partial * 2) 1 (partial < 0))))))

(deftest test-iterates-while
  (testing "Get all iterates while a predicate is true"
    (is (= (range 5) (hcore/iterates-while inc 0 (partial >= 4))))))

(deftest test-reverse-args
  (testing "We can reverse the arguments of functions."
    (let [sub (fn [x y] (- x y))
          f (fn [x y z] (* z (- x y)))]
      (is (= 3 ((hcore/reverse-args sub) 4 7)))
      (is (= -4 ((hcore/reverse-args f) 4 7 6))))))

(deftest test-irange
  (testing "Inclusive range"
    (is (= (range 10) (hcore/irange 9)))
    (is (= (range 1 20) (hcore/irange 1 19)))
    (is (= (range 1 20 2) (hcore/irange 1 19 2)))
    (is (= (range 1 20 3) (hcore/irange 1 19 3)))))

