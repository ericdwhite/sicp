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
; 2.1.1 Arithmetic Operations on Rational Numbers
;
(defn make-rat [n d]
  (let [g (math/gcd n d)]
    [(/ n g) (/ d g)]))

(defn numer [r]
  (first r))

(defn denom [r]
  (second r))

(defn add-rat [r1 r2]
  (make-rat (+ (* (numer r1) (denom r2))
               (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(defn sub-rat [r1 r2]
  (make-rat (- (* (numer r1) (denom r2))
               (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(defn mul-rat [r1 r2]
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))

(defn div-rat [r1 r2]
  (make-rat (* (numer r1) (denom r2))
            (* (denom r1) (numer r2))))

(defn equal-rat? [r1 r2]
  (= (* (numer r1) (denom r2))
     (* (numer r2) (denom r1))))

;
; 2.2.1 Representing Sequences
;
(defn length [items]
  (letfn [(length-iter [i acc]
            (if (empty? i)
              acc
              (recur (rest i) (+ 1 acc))))]
    (length-iter items 0)))

;
; 2.2.2 Hierarchical Structures
;
(defn count-leaves [x]
  (cond
    (not (coll? x)) 1
    (empty? x) 0
    :else (+ (count-leaves (first x))
             (count-leaves (rest x)))))

;
; 2.2.3 Sequences as Conventional Interfaces
;
(defn sicp-filter [p seqn]
  (cond
    (and (coll? seqn) (empty? seqn)) nil
    (p (first seqn)) (cons (first seqn) (sicp-filter p (rest seqn)))
    :else (recur p (rest seqn))))

(defn accumulate [op initial seqn]
  (println initial seqn)
  (if (and (coll? seqn) (empty? seqn))
    initial
    (op (first seqn)
        (accumulate op initial (rest seqn)))))

;
; Scratch
;

