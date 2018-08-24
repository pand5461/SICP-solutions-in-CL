;;; Exercise 1.12

(defun pascal-triangle (row num)
  (cond ((> num row) nil)
	((= num 1) 1)
	((= num row) 1)
	(t (+ (pascal-triangle (1- row) num)
	      (pascal-triangle (1- row) (1- num))))))
	
