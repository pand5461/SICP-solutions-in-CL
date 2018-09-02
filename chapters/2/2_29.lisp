;;; Exercise 2.29

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

;; a
(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cadr mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cadr branch))

;; b
(defun branch-weight (branch)
  (total-weight (branch-structure branch)))

(defun total-weight (mobile)
  (cond ((null mobile) 0)
	((atom mobile) mobile)
	(t (+ (branch-weight (left-branch mobile))
	      (branch-weight (right-branch mobile))))))

;; c.
(defun weight-and-balance (struct)
  (cond ((null struct) (cons 0 t))
	((atom struct) (cons struct t))
	(t
	 (let ((left-wb (weight-and-balance (branch-structure (left-branch struct))))
	       (right-wb (weight-and-balance (branch-structure (right-branch struct))))
	       (left-arm (branch-length (left-branch struct)))
	       (right-arm (branch-length (right-branch struct))))
	   (cons (+ (car left-wb)
		    (car right-wb))
		 (and (cdr left-wb)
		      (cdr right-wb)
		      (= (* left-arm (car left-wb))
			 (* right-arm (car right-wb)))))))))

(defun balancedp (mobile)
  (cdr (weight-and-balance mobile)))
