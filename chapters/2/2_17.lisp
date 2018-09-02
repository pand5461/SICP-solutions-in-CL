;;; Exercise 2.17

(defun last-pair (lst)
  (cond ((null lst) nil)
	((atom (cdr lst)) lst)
	(t (last-pair (cdr lst)))))
