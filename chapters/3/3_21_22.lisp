;;; Exercises 3.21 and 3.22

(defun make-queue ()
  (cons nil nil))

(defun queue-is-empty? (queue)
  (null (front-ptr queue)))

(defun front-ptr (queue)
  (car queue))

(defun rear-ptr (queue)
  (cdr queue))

(defun queue-set-front-ptr! (queue item)
  (setf (car queue) item))

(defun queue-set-rear-ptr! (queue item)
  (setf (cdr queue) item))

(defun queue-front (queue)
  (if (queue-is-empty? queue)
      (error "QUEUE-FRONT called with an empty queue ~S" queue)
      (car (front-ptr queue))))

(defun queue-push! (queue item)
  (let ((new-pair (cons item nil)))
    (cond ((queue-is-empty? queue)
	   (queue-set-front-ptr! queue new-pair)
	   (queue-set-rear-ptr! queue new-pair)
	   queue)
	  (t
	   (setf (cdr (rear-ptr queue)) new-pair)
	   (queue-set-rear-ptr! queue new-pair)
	   queue))))

(defun queue-delete! (queue)
  (cond ((queue-is-empty? queue)
	 (error "QUEUE-DELETE! called on an empty queue ~S" queue))
	(t (queue-set-front-ptr! queue (cdr (front-ptr queue)))
	   queue)))

;; 3.21
;; only the CAR of queue represents its contents
;; the CDR is just a pointer to the last element
(defun queue-print (queue)
  (format t "~a~%" (front-ptr queue)))

;; 3.22
;; calls look really ugly
;; (defvar *Q* (make-queue-func))
;; (funcall (funcall *Q* 'push!) 1)
;; (funcall (funcall *Q* 'push!) 2)
;; (funcall (funcall *Q* 'print))
(defun make-queue-func ()
  (let ((front-ptr nil)
	(rear-ptr nil))
    (labels
	((empty? () (null front-ptr))
	 
	 (front-ptr () front-ptr)
	 
	 (rear-ptr () rear-ptr)
	 
	 (set-front-ptr! (item)
	   (setf front-ptr item))
	 
	 (set-rear-ptr! (item)
	   (setf rear-ptr item))
	 
	 (front ()
	   (if (empty?)
	       (error "QUEUE-FRONT called with an empty queue")
	       (car front-ptr)))

	 (push! (item)
	     (let ((new-pair (cons item nil)))
	       (cond ((empty?)
		      (set-front-ptr! new-pair)
		      (set-rear-ptr! new-pair))
		     (t
		      (setf (cdr rear-ptr) new-pair)
		      (set-rear-ptr! new-pair)))))

	 (delete! () 
	     (cond ((empty?)
		    (error "QUEUE-DELETE! called on an empty queue"))
		   (t (set-front-ptr! (cdr front-ptr)))))

	 (qprint ()
	   (format t "~a~%" (front-ptr)))

	 (dispatch (op &rest args)
	   (case op
	     ('empty? #'empty?)
	     ('front-ptr #'front-ptr)
	     ('rear-ptr #'rear-ptr)
	     ('set-front-ptr! #'set-front-ptr!)
	     ('set-rear-ptr! #'set-rear-ptr!)
	     ('front #'front)
	     ('push! #'push!)
	     ('delete! #'delete!)
	     ('print #'qprint)
	     (otherwise (error "Unknown queue operation")))))
      #'dispatch)))

