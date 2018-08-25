;;; Exercises 1.29 and 1.30

;; #'sum rewritten in terms of tail recursion for 1.30
(defun sum (term a next b)
  (labels ((sum-iter (a acc)
	     (if (> a b)
		 acc
		 (sum-iter (funcall next a)
			   (+ acc (funcall term a))))))
    (sum-iter a 0)))

(defun int-midpoint (fn a b nseg)
  (labels ((dx ()
	     (/ (- b a) nseg 1.0d0)))
    (* (sum fn
	    (+ a (* (dx) 0.5d0))
	    #'(lambda (x) (+ x (dx)))
	    b)
       (dx))))

(defun int-trapezoid (fn a b nseg)
  (labels ((dx ()
	     (/ (- b a) nseg 1.0d0)))
    (* (+ (* 0.5d0 (+ (funcall fn a)
		      (funcall fn b)))
	  (sum fn
	       (+ a (dx))
	       #'(lambda (x) (+ x (dx)))
	       (- b (/ (dx) 2.0d0))))
       (dx))))

(defun cube (x) (* x x x))

;; Define Simpson's rule as the combination of trapezoid and midpoint rules
;; for the sake of reducing roundoff error
(defun int-simpson (fn a b nseg)
  (labels ((nseg-corr ()
	     (/ (if (oddp nseg)
		    (+ nseg 1)
		    nseg)
		2)))
    (/ (+ (int-trapezoid fn a b (nseg-corr))
	  (* 2 (int-midpoint fn a b (nseg-corr))))
       3.0d0)))

;; Simpson's rule is theoretically absolutely accurate for polynomial of degree <= 3
(format t "Simpson's rule integration of x^3 from 0 to 1:~%")
(format t "N = 10: ~a~%" (int-simpson #'cube 0 1 10))
(format t "N = 100: ~a~%" (int-simpson #'cube 0 1 100))
(format t "N = 1000: ~a~%" (int-simpson #'cube 0 1 1000))
