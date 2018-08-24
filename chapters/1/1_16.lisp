;;; Exercise 1.16

(defun fast-expt (b n)
  "Raises b to the power n"
  (labels ((expt-iter (acc mult n)
	     "Returns acc * mult^n"
	     (cond ((zerop n) acc)
		   ((evenp n) (expt-iter acc (* mult mult) (/ n 2)))
		   (t (expt-iter (* acc mult) mult (- n 1))))))
    (expt-iter 1 b n)))
