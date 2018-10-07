;;; Exercise 3.8

(let ((ncalls -1))
  (defun order-matters (x)
    (setf ncalls (1+ ncalls))
    (if (zerop ncalls)
	x
	0)))
