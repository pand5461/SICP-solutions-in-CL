;;; Exercise 1.3
(load "SICP-utils")

(use-package :sicp-utils)

(defun sum-two-largest-squares (a b c)
"Procedure takes three numbers and returns the sum of the squares of the two larger numbers"
  (if (>= a b)
      (+ (square a)
	 (if (>= b c)
	     (square b)
	     (square c)))
      (+ (square b)
	 (if (>= a c)
	     (square a)
	     (square c)))))
