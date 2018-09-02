;;; Exercise 2.31

(defun tree-map (fn tree)
  (cond ((null tree) nil)
	((atom tree) (funcall fn tree))
	(t (cons (tree-map fn (car tree))
		 (tree-map fn (cdr tree))))))
