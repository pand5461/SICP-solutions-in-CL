;;; Exercises 2.59, 2.61, 2.62

;; basic definitions from chapter 2.3.3

(defun element-of-set? (x set)
  (cond ((null set) nil)
	((equal x (car set)) t)
	(t (element-of-set? x (cdr set)))))

(defun set-adjoin (x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(defun set-intersection (set1 set2)
  (cond ((or (null set1) (null set2)) nil)
	((element-of-set? (car set1) set2)
	 (cons (car set1) (set-intersection (cdr set1) set2)))
	(t (set-intersection (cdr set1) set2))))

;; 2.59
(defun set-union (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	(t (set-union (cdr set1) (adjoin-set x set2)))))

;; sets as ordered lists
(defun element-of-set? (x set)
  (cond ((null set) nil)
	((= x (car set)) t)
	((< x (car set)) nil)
	(t (element-of-set? x (cdr set)))))

(defun set-intersection (set1 set2)
  (if (or (null set1) (null set2))
      ()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (set-intersection (cdr set1)
					  (cdr set2))))
	      ((< x1 x2)
	       (set-intersection (cdr set1) set2))
	      (t
	       (set-intersection set1 (cdr set2)))))))

;; 2.61
(defun set-adjoin (x set)
  (cond ((or (null set) (< x (car set))) (cons x set))
	((= x (car set)) set)
	(t (cons (car set) (set-adjoin x (cdr set))))))

;; 2.62
(defun set-union (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	(t
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (set-union (cdr set1)
				      (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (set-union (cdr set1) set2)))
		 (t
		  (cons x2 (set-union set1 (cdr set2)))))))))
