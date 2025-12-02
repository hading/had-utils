(ns had-utils.string-test
  (:require [clojure.test :refer :all]
            [had-utils.string :as hstr]
            [clojure.string :as str]))

(deftest test-split*
  (testing "Convenience split* with arguments reversed"
    (let [s "joe bob pete"
          re #"\s"
          limit 2]
      (is (= (str/split s re) (hstr/split* re s)))
      (is (= (str/split s re limit) (hstr/split* re s limit))))))
