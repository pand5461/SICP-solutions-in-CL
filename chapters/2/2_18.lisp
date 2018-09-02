;;; Exercise 2.18

(defun my-reverse (lst)
  (labels ((iterator (acc lst)
	     (if (null lst)
		 acc
		 (iterator (cons (car lst)
				 acc)
			   (cdr lst)))))
    (iterator nil lst)))
