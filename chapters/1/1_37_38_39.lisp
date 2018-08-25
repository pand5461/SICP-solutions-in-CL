;;; Exercises 1.37 - 1.39

;; Continued fraction evaluation is controlled by relative tolerance
;; Uses modified Lentz's method (W.H. Press et al., Numerical Recipes)
;; A and B in #'iterator are C_i and D_i from NR
;; N and D are a_i and b_i from NR
(defun cont-frac (n d rtol)
  (let ((tiny 1d-30)) 
    (labels ((limit-abs (x)
	       (if (zerop x)
		   tiny
		   x))
	     (next-a (a index)
	       (limit-abs (+ (funcall d index)
			     (/ (funcall n index)
				a))))
	     (next-b (b index)
	       (/ 1 (limit-abs (+ (funcall d index)
				  (* (funcall n index)
				     b)))))
	     (iterator (ans a b index)
	       (let* ((an (next-a a index))
		      (bn (next-b b index))
		      (delta (* an bn)))
		 (format t "Fraction depth: ~a~%" index)
		 (if (< (abs (- delta 1)) rtol)
		     (* ans delta)
		     (iterator (* ans delta)
			       an
			       bn
			       (1+ index))))))
      (let ((f0 (limit-abs (* 1d0 (funcall d 0)))))
	(iterator f0 f0 0 1)))))
  
;; Computing approximation to phi (1.37)
(format t "Calculating the golden ratio...~%")
(format t "Golden ratio approximation to 4 decimal digits is ~,4e~%"
	(cont-frac #'(lambda (i) 1d0)
		   #'(lambda (i) (if (zerop i) 0 1d0))
		   1d-4))

;; Computing approximation to Euler's number (1.38)
(format t "Calculating the Euler's number...~%")
(format t "Euler's number approximation to 4 decimal digits is ~,4e~%"
	(cont-frac #'(lambda (i) 1d0)
		   #'(lambda (i) (if (zerop i)
				     2
				     (multiple-value-bind (fr rem) (truncate i 3)
				       (if (= rem 2) (* (1+ fr) 2) 1))))
		   1d-4))

;; Tangent function (1.39)
(defun tan-cf (x)
  (cont-frac #'(lambda (i) (if (> i 1) (- (* x x)) x))
	     #'(lambda (i) (if (zerop i) 0 (+ i i -1)))
	     1d-16))
