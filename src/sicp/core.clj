(ns sicp.core
  (:require [clojure.math.numeric-tower :as math]))

;
; 1.1.7 Square Roots by Newton's Method
;
(defn square [x]
  (* x x))

(defn sqrt [x]
  (sqrt-iter 1 x))

(defn average [a b]
  (/ (+ a b) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-iter [guess x]
  (letfn [(good-enough? [guess x]
            (<= (math/abs (- (square guess) x)) 0.001))]
    (if (good-enough? guess x)
      guess
      (recur (improve guess x)
             x))))

;
; 1.3.1 Higher Order Procedures
;
(defn cube [x]
  (* x x x))

(defn sum [term a step b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (step a) step b))))

(defn sum-integers [a b]
  (sum identity a inc b))

(defn sum-cubes [a b]
  (sum cube a inc b))


