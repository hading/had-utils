(ns had-utils.io-test
  (:require [clojure.test :refer :all]
            [had-utils.io :as hio]
            [clojure.string :as str]
            [clojure.java.io :as jio]))

(deftest test-line->ints
  (testing "Can get a map of integers from a string."
    (is (= [103 49 28] (hio/line->ints "103 49 28"))))
  (testing "Can get a map of integers from a string with weirder separators."
    (is (= [103 49 28] (hio/line->ints "103\tdkfjldkf49kldfjkdl28")))))

(deftest test-slurp-grid
  (testing "Can get file as a grid"
    (is (= [["1" "2" "3"] ["A" "B" "C"] ["+" "-" "*"]]
           (hio/slurp-grid (jio/resource "grid.txt")))))
    (testing "Can get file as a grid and transform entries with a function"
    (is (= [["1" "2" "3"] ["a" "b" "c"] ["+" "-" "*"]]
           (hio/slurp-grid (jio/resource "grid.txt") str/lower-case)))))

(deftest test-slurp-lines
  (testing "Read test file"
    (is (= ["There" "are" "" "some" "words" "" "here"]
           (hio/slurp-lines (jio/resource "sample.txt"))))))

(deftest test-slurp-lines-map
  (testing "Count letters in lines"
    (is (= [5 3 0 4 5 0 4]
           (hio/slurp-lines-map (jio/resource "sample.txt") count)))))

(deftest test-slurp-split
  (testing "Read test file with \\n\\n"
    (is (= ["There\nare" "some\nwords" "here\n"]
           (hio/slurp-split (jio/resource "sample.txt") #"\n\n")))))

(deftest test-slurp-split-map
  (testing "Read test file with \\n\\n"
    (is (= ["THERE\nARE" "SOME\nWORDS" "HERE\n"]
           (hio/slurp-split-map (jio/resource "sample.txt") #"\n\n" str/upper-case)))))
