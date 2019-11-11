(let ((prev-file "./1_1_7_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.6

;; This will not work: #'if is a special form for a reason
;; Both arguments of new-if must be evaluated before executing the function
(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
	(t else-clause)))

;; With #'new-if not being a special form, LISP will always try to evaluate its third argument,
;; which in this case will result in an endless loop
(defun sqrt-iter-ex-1-6 (guess x)
  (labels ((good-enough? (guess x)
	     (< (abs (- x (square guess))) 0.001d0))
	   (improve (guess x)
	     (average guess (/ x guess))))
    (new-if (good-enough? guess x)
	    guess
	    (sqrt-iter-ex-1-6 (improve guess x) x))))

;;(sqrt-iter 1d0 2) causes call stack overflow error

;;; Exercise 1.7

(defun good-enough? (guess prev-guess)
  (within-rtol? guess prev-guess 0.001d0))

(defun within-rtol? (test-value ref-value rtol)
  (<= (abs (- test-value ref-value))
      (* rtol ref-value)))

(defun sqrt-iter (guess prev-guess x)
  (cond
    ((= x 0) 0d0)
    ((good-enough? guess prev-guess) guess)
    (t (sqrt-iter (improve guess x) guess x))))

(defun sqrt1 (x)
  "Computes an approximation to the square root of X"
  (sqrt-iter 1.0d0 x x))

;;; Exercise 1.8

(defun cuberoot-iter (guess prev-guess x)
  (cond
    ((= x 0) 0d0)
    ((good-enough? guess prev-guess) guess)
    (t (cuberoot-iter (improve-cbrt guess x) guess x))))

;; In the spirit of the previous exercise, expressing (x/y^2 + 2y)/3
;; as the average of (x/y^2, y, y)
(defun improve-cbrt (guess x)
  (average guess guess (/ x (square guess))))

(defun cuberoot (x)
  "Computes an approximation to the cube root of X"
  (cuberoot-iter 1.0d0 x x))

(defun ex-1-8 ()
  (format t
          "Test on a small number: CUBEROOT(1.0d-6)=~a~%"
          (cuberoot 1.0d-6))
  (format t
          "Test on a large number: CUBEROOT(1.0d+9)=~a~%"
          (cuberoot 1.0d9)))
