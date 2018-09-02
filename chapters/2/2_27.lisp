;;; Exercise 2.27

(defun deep-reverse (tree)
  (labels ((rev (items acc)
	     (cond ((null items) acc)
		   ((atom items) items)
		   (t (rev (cdr items)
			   (cons (rev (car items) nil)
				 acc))))))
    (rev tree nil)))
