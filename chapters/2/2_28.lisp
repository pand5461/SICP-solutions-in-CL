;;; Exercise 2.28

(defun deep-reverse (tree)
  (cond ((atom tree) tree)
	(t (append (deep-reverse (cdr tree))
		   (list (deep-reverse (car tree)))))))
