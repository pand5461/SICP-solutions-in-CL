;;; Exercise 1.6

; This will not work: #'if is a special form for a reason
; Both arguments of new-if must be evaluated before executing the function
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

; With #'new-if not being a special form, LISP will always try to evaluate its third argument,
; which in this case will result in an endless loop
(defun sqrt-iter (guess x)
  (labels ((good-enough? (guess x)
	     (< (abs (- x guess)) 0.0001))
	   (improve (guess x)
	     (/ (+ guess (/ x guess)) 2)))
    (new-if (good-enough? guess x)
	    guess
	    (sqrt-iter (improve guess x)))))
