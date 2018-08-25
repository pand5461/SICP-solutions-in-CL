;;; Exercise 1.36

(defun average (&rest args)
  (labels ((ave-iter (acc lst nelts)
	     (if (null lst)
		 (/ acc nelts)
		 (ave-iter (+ (car lst) acc)
			   (cdr lst)
			   (1+ nelts)))))
    (if (null args)
	0
	(ave-iter 0 args 0))))

(defun fixed-point (f first-guess)
  (let ((rtol 1d-6))
    (labels ((close-enough? (v1 v2)
	       (< (abs (- v1 v2)) (* rtol (average v1 v2))))
	     (try (guess ntry)
	       (let ((next (funcall f guess)))
		 (format t "Approximation #~a is: ~a~%" ntry next)
		 (cond ((close-enough? guess next)
			(format t "Approximation accepted~%")
			next)
		       (t (try next (1+ ntry)))))))
      (format t "Starting guess: ~a~%" first-guess)
      (try (* 1d0 first-guess) 1))))

;; Define average damping as a higher-order function
(defun ave-damp (f)
  (lambda (x) (average x (funcall f x))))

;; Compare the search of fixed point with and without average damping
(format t "Solving x^x = 1000 by fixed point x = log(1000) / log(x)~%")
(format t "Without average damping:~%")
(fixed-point #'(lambda (x) (/ (log 1d3) (log x))) 2)
(format t "With average damping:~%")
(fixed-point (ave-damp #'(lambda (x) (/ (log 1d3) (log x)))) 2)
