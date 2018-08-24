;;; Exercise 1.7

(defun average (x y)
  (/ (+ x y) 2))

(defun mysqrt (x)
  (labels ((sqrt-iter (guess)
	     (if (good-enough? guess (improve guess))
		 guess
		 (sqrt-iter (improve guess))))
	   ;; improved quality estimator; uses relative difference between subsequent approximations
	   (good-enough? (guess oldguess &optional (rtol 1.0d-6))
	     (<= (abs (- guess oldguess)) (* rtol guess)))
	   (improve (guess)
	     (average guess (/ x guess))))
    (if (zerop x)
	0.0d0
	(sqrt-iter 1.0d0))))

(format t "Test on a very small number: MYSQRT(1.0d-6)=~a~%" (mysqrt 1.0d-6))
(format t "Test on a very large number: MYSQRT(1.0d+8)=~a~%" (mysqrt 1.0d8))
