;;; Exercise 3.1

(defun make-accumulator (initial-value)
  (let ((x0 initial-value))
    #'(lambda (x)
	(setf x0 (+ x x0))
	x0)))
