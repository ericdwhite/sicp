(ns sicp.core
  (:require [clojure.math.numeric-tower :as math]))

;
; 1.1.7 Square Roots by Newton's Method
;
(defn square [x]
  (* x x))

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

(defn sqrt [x]
  (sqrt-iter 1 x))

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

; Alternate function definition
(defn sum-pi [a b]
  (def pi-term (fn [x]
                 (/ 1.0 (* x (+ x 2)))))
  (def pi-step (fn [x]
                 (+ x 4)))
  (sum pi-term a pi-step b))

;
; 1.3.4 Procedures Returned as Values
;
(defn average-damp [f]
  (fn [x] (average x (f x))))

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [a b]
            (< (math/abs (- a b)) 0.00001))
          (try-guess [guess]
            (let [step (f guess)]
              (if (close-enough? guess step)
                step
                (try-guess step))))]
    (try-guess first-guess)))

(defn sqrt2 [x]
  (fixed-point (average-damp (fn [y] (/ x y))) 1.0))

;
; Scratch
;
