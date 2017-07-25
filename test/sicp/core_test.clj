(ns sicp.core-test
  (:require [clojure.test :refer :all]
            [sicp.core :refer :all]))

; SICP tests
;
;    (sicp.core-test/test-sicp)
;
(defn test-sicp []
  (run-tests 'sicp.core-test))

(deftest test-1-1-7
  (testing "Square Root by Netwon's Method"
    (is (= 2.0000000929222947 (sqrt 4.0)))))

(deftest test-1-3-1
  (testing "Higher Order Procedures"
    (is (= 6 (sum-integers 1 3)))
    (is (= 36 (sum-cubes 1 3)))))

(deftest test-1-3-4
  (testing "Procedures Returned as Values"
    (is (= 2.000000000000002 (sqrt2 4.0)))))

(deftest test-2-1-1
  (testing "Arithmetic Operations on Rational Numbers"
    (is  (= (make-rat 1 2) (make-rat 2 4)))
    (is  (= (make-rat 1 2) (add-rat (make-rat 1 4) (make-rat 1 4))))
    (is  (= (make-rat 1 2) (sub-rat (make-rat 3 4) (make-rat 1 4))))
    (is  (= (make-rat 1 4) (mul-rat (make-rat 2 4) (make-rat 1 2))))
    (is  (= (make-rat 1 2) (div-rat (make-rat 1 1) (make-rat 2 1))))
    ))

(deftest test-2-2-1
  (testing "Representing Sequences"
    (is (= 0 (length [])))
    (is (= 2 (length [1 2])))
    (is (= 3 (length (list 1 2 3))))
    ))

(deftest test-2-2-2
  (testing "Hierarchical Structures"
    (let [y (cons [1 2][3 4])]
      (is (= 4 (count-leaves y))))
    ))

(deftest test-2-2-3
  (testing "Sequences as Conventional Interfaces"
    (is (= '(1 3 5 7) (sicp-filter odd? [1 2 3 4 5 6 7 8])))
    ))

