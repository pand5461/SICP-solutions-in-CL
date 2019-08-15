;;; Exercise 1.8

(load "SICP-utils")
(use-package :sicp-utils)

(defun cuberoot (x)
  (labels ((cuberoot-iter (guess)
	     (if (good-enough? guess (improve guess))
		 guess
		 (cuberoot-iter (improve guess))))

	   (good-enough? (guess oldguess &optional (rtol 1.0d-6))
	     (<= (abs (- guess oldguess)) (* rtol guess)))

	   (square (x) (* x x))

	   ;; In the spirit of the previous exercise, expressing (x/y^2 + 2y)/3
	   ;; as the average of (x/y^2, y, y)
	   (improve (guess)
	     (average guess guess (/ x (square guess)))))

    (if (zerop x)
	0.0d0
	(cuberoot-iter 1.0d0))))

(format t "Test on a small number: CUBEROOT(1.0d-6)=~a~%" (cuberoot 1.0d-6))
(format t "Test on a large number: CUBEROOT(1.0d+9)=~a~%" (cuberoot 1.0d9))
