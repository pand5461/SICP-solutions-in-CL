(defun make-deque ()
  (cons nil nil))

(defun deque-empty? (deque)
  (and (null (car deque))
       (null (cadr deque))))

(defun deque-front-ptr (deque)
  (car deque))

(defun deque-rear-ptr (deque)
  (cdr deque))

(defun deque-front (deque)
  (cond ((deque-empty? deque)
	 (error "Cannot get element from an empty deque: DEQUE-FRONT!"))
	(t
	 (caar deque))))

(defun deque-rear (deque)
  (cond ((deque-empty? deque)
	 (error "Cannot get element from an empty deque: DEQUE-REAR!"))
	(t
	 (cadr deque))))

(defun deque-insert-front! (deque new-item)
  (let ((new-elt (list new-item nil)))
    (cond ((deque-empty? deque)
	   (setf (car deque) new-elt)
	   (setf (cdr deque) new-elt))
	  (t
	   (setf (cddr new-elt) (deque-front-ptr deque))
	   (setf (cadr (deque-front-ptr deque)) new-elt)
	   (setf (car deque) new-elt)))
    deque))

(defun deque-insert-rear! (deque new-item)
  (let ((new-elt (list new-item nil)))
    (cond ((deque-empty? deque)
	   (setf (car deque) new-elt)
	   (setf (cdr deque) new-elt))
	  (t
	   (setf (cadr new-elt) (deque-rear-ptr deque))
	   (setf (cddr (deque-rear-ptr deque)) new-elt)
	   (setf (cdr deque) new-elt)))
    deque))

(defun deque-print (deque)
  (labels ((rec-print (lst firstcall)
	     (cond ((null lst)
		    (format t ")~%"))
		   (firstcall
		    (format t "~A" (car lst))
		    (rec-print (cadr lst) nil))
		   (t
		    (format t " ~A" (car lst))
		    (rec-print (cadr lst) nil)))))
    (format t "#DQ(")
    (rec-print (deque-rear-ptr deque) t)))

(defun deque-delete-front! (deque)
  (cond ((deque-empty? deque)
	 (error "Cannot delete element from an empty deque: DEQUE-DELETE-FRONT!"))
	(t
	 (setf (car deque) (cddr (deque-front-ptr deque)))
	 (if (car deque)
	     (setf (cadr (deque-front-ptr deque)) nil)
	     (setf (cdr deque) nil))
	 deque)))

(defun deque-delete-rear! (deque)
  (cond ((deque-empty? deque)
	 (error "Cannot delete element from an empty deque: DEQUE-DELETE-REAR!"))
	(t
	 (setf (cdr deque) (cadr (deque-rear-ptr deque)))
	 (if (deque-rear-ptr deque)
	     (setf (cddr (deque-rear-ptr deque)) nil)
	     (setf (car deque) nil))
	 deque)))
