;;; Exercise 3.2

(defun make-monitored (fn)
  (let ((ncalls 0))
    (labels ((monitored-function (&rest args)
	       (cond ((eq args '(how-many-calls?))
		      ncalls)
		     ((eq args '(reset-count))
		      (setf ncalls 0))
		     (t (setf ncalls (1+ ncalls))
			(apply fn args)))))
      #'monitored-function)))
