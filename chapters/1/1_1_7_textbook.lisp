(let ((prev-file "./1_1_5_textbook.lisp"))
  (load prev-file))

(in-package :sicp)

(defun average (x &rest rest-numbers)
  "Averages the numbers passed as input"
  (labels ((ave-tco (sum nargs tco-args)
	       (if (null tco-args)
		   (/ sum nargs)
		   (ave-tco (+ sum (car tco-args))
			    (1+ nargs)
			    (cdr tco-args)))))
    (ave-tco x 1 rest-numbers)))

(defun sqrt-iter (guess x)
  (cond
    ((= x 0) 0d0)
    ((good-enough? guess x) guess)
    (t (sqrt-iter (improve guess x) x))))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001d0))

(defun sqrt1 (x)
  "Computes an approximation to the square root of X"
  (sqrt-iter 1.0d0 x))

(export '(average improve sqrt1) :sicp)
