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
