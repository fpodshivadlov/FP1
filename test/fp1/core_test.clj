(ns fp1.core-test
  (:require [clojure.test :refer :all]
            [fp1.core :refer :all]))

(deftest distance-test
  (testing "euclideanDistance"
    (is (= (euclideanDistance '(0 0) '(3.0 4.0)) 5.0))
    (is (= (euclideanDistance '(0 0) '(6.0 8.0)) 10.0))
  )
  (testing "hammingDistance"
    (is (= (hammingDistance '(0 0) '(8.0 8.0)) 2.0))
    (is (= (hammingDistance '(0 0) '(0.0 8.0)) 1.0))
    (is (= (hammingDistance '(0 0) '(0.0 0.0)) 0.0))
  )
)

(deftest initialize-test
  (testing "convertToInputData"
    (is (= (convertToInputData [["0" "0" "1"]]) '({:values (0.0 0.0), :label "1"})))
    (is (= (convertToInputData []) '()))
  )
)
