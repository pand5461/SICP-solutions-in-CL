;;; Exercise 2.20

(defun same-parity (n1 &rest numbers)
  (labels ((iterator (acc nums)
	     (if (null nums)
		 (reverse acc)
		 (let ((n2 (car nums)))
		   (if (evenp (- n1 n2))
		       (iterator (cons n2 acc) (cdr nums))
		       (iterator acc (cdr nums)))))))
    (iterator (list n1) numbers)))
