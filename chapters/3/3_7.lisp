;;; Exercise 3.7

(defun make-account (initial-balance password)
  (let ((balance initial-balance)
	(n-incorrect-pass 0)
	(max-incorrect-times 7))
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn
		   (setf balance (- balance amount))
		   balance)
		 "Insufficient funds"))
	   (deposit (amount)
	     (setf balance (+ balance amount))
	     balance)
	   (reset-try ()
	     (setf n-incorrect-pass 0))
	   (raise-alarm ()
	     (print "Raised alarm"))
	   (wrong-password (pas)
	     (not (equal pas password)))
	   (dispatch (pas action)
	     (cond ((wrong-password pas)
		    (setf n-incorrect-pass (1+ n-incorrect-pass))
		    (if (>= n-incorrect-pass max-incorrect-times)
			(raise-alarm))
		    #'(lambda (x) "Incorrect password"))
		   ((eq action 'withdraw)
		    (reset-try)
		    #'withdraw)
		   ((eq action 'deposit)
		    (reset-try)
		    #'deposit)
		   (t (error "Unknown request: ~S: MAKE-ACCOUNT" action)))))
    #'dispatch)))

(defun make-joint (account orig-pass new-pass)
  (labels ((sec-dispatch (pas action)
	     (cond ((not (equal pas new-pass))
		    #'(lambda (x) "Incorrect password"))
		   (t (funcall account orig-pass action)))))
    ;; try depositing a zero sum using orig-pass
    ;; on success, return the linked account
    ;; else signal an error
    (if (numberp
	 (funcall
	  (funcall account orig-pass 'deposit)
	  0))
	#'sec-dispatch
	(error "Original password is incorrect : MAKE-JOINT"))))
