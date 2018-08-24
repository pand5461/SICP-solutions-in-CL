;;; Exercise 1.3

(defun sum-two-largest-squares (a b c)
"Procedure takes three numbers and returns the sum of the squares of the two larger numbers"
  (if (>= a b)
      (+ (* a a)
	 (if (>= b c)
	     (* b b)
	     (* c c)))
      (+ (* b b)
	 (if (>= a c)
	     (* a a)
	     (* c c)))))
