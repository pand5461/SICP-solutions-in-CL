;;; Exercises 3.16 and 3.17

(defun bad-count-pairs (x)
  (if (not (consp x))
      0
      (+ (bad-count-pairs (car x))
	 (bad-count-pairs (cdr x))
	 1)))

(defparameter *z* (list 'a 'b 'c))

(bad-count-pairs *z*) ;; 3

(setf (cadr *z*) (cddr *z*)) ;; *z* is now (a (c) c)

(bad-count-pairs *z*) ;; 4

(setf (car *z*) (cdr *z*)) ;; *z* is now (((c) c) (c) c)

(bad-count-pairs *z*) ;; 7

(defun good-count-pairs (x)
  (let ((unique-pairs nil))
    (labels ((helper (x)
	       (cond ((not (consp x)) 0)
		     ((member x unique-pairs :test #'eq) 0)
		     (t
		      (setf unique-pairs (cons x unique-pairs))
		      (+ (helper (car x))
			 (helper (cdr x))
			 1)))))
      (helper x))))

(good-count-pairs *z*) ;; 3
