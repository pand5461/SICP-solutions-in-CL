;;; Exercise 1.15

(load "SICP-utils")
(use-package :sicp-utils)

(defun p (x) (* x (- 3 (* 4 (square x)))))

(defun sine (angle)
  (if (<= (abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0)))))

(trace p)

(sine 12.15)

(untrace)

;; from the trace, it is seen that #'p is called 5 times
;; overall order of growth O(ln a), as log_3(a / 0.1) steps reduce a to a value around ~0.1
