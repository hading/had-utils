(ns had-utils.io-test
  (:require [clojure.test :refer :all]
            [had-utils.io :as hio]))


(deftest test-slurp-split
  (testing "Read test file with \\n\\n"
    (is (= ["There\nare" "some\nwords" "here\n"]
           (hio/slurp-split "test/had_utils/sample.txt" #"\n\n")))))

(deftest test-slurp-lines-map
  (testing "Count letters in lines"
    (is (= [5 3 0 4 5 0 4]
           (hio/slurp-lines-map "test/had_utils/sample.txt" count)))))

(deftest test-slurp-lines
  (testing "Read test file"
    (is (= ["There" "are" "" "some" "words" "" "here"]
           (hio/slurp-lines "test/had_utils/sample.txt")))))
