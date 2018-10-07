;;; Exercises 3.18 and 3.19

(defun circular? (lst)
  (let ((unique-pairs nil))
    (labels ((iter (l)
	       (cond ((null l) nil)
		     ((member l unique-pairs :test #'eq) t)
		     (t
		      (setf unique-pairs (cons l unique-pairs))
		      (iter (cdr l))))))
      (iter lst))))

(defun constant-space-circular? (lst)
  (labels ((iter (l1 l2)
	     (cond ((eq l1 l2) t)
		   ((null l2) nil)
		   (t (iter (cdr l1) (cddr l2))))))
    ;; l1 goes 1 item down the list at a time
    ;; l2 goes 2 items down the list at a time
    ;; if there is a circle, they ought to meet at some point
    (iter lst (cdr lst))))
