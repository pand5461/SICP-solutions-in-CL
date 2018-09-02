;;; Exercises 2.40 and 2.41

;; translation of textbook functions into CL
(defun accumulate (op null-elt seq)
  (if (null seq)
      null-elt
      (funcall op
	       (car seq)
	       (accumulate op null-elt (cdr seq)))))

(defun enumerate-interval (num1 num2)
  (labels ((iter (acc upper-bound)
	     (if (< upper-bound num1)
		 acc
		 (iter (cons upper-bound acc)
		       (1- upper-bound)))))
    (iter nil (truncate num2))))

(defun flatmap (op seq)
  (accumulate #'append nil (mapcar op seq)))

;; 2.40
(defun unique-pairs (n)
  (flatmap #'(lambda (k)
	       (mapcar #'(lambda (j) (list k j))
		       (enumerate-interval 1 (1- k))))
	   (enumerate-interval 2 n)))

;; 2.41
(defun unique-triples (n)
  (flatmap #'(lambda (k)
	       (mapcar #'(lambda (j) (cons k j))
		       (unique-pairs (1- k))))
	   (enumerate-interval 3 n)))

(defun triples-with-given-sum (n sum)
  (remove-if-not #'(lambda (lst) (= (accumulate #'+ 0 lst)
				    sum))
		 (unique-triples n)))

;; 2.42
(defun queens (board-size)
  (let ((empty-board nil))
    (labels ((queen-cols (k)
	       (if (zerop k)
		   (list empty-board)
		   (remove-if-not
		    #'(lambda (positions)
			(safep k positions))
		    (flatmap
		     #'(lambda (rest-of-queens)
			 (mapcar #'(lambda (new-row)
				     (adjoin-position
				      new-row k rest-of-queens))
				 (enumerate-interval 1 board-size)))
		     (queen-cols (1- k))))))
	     (adjoin-position (new-row k rest-of-queens)
	       (cons (cons new-row k) rest-of-queens))  ;; last added position is first in the list
	     (safep (k positions)
	       (cond ((null (cdr positions)) t)         ;; only one position - safe
		     ((= (caar positions)
			 (caadr positions))             ;; same rows of 1st and 2nd - unsafe
		      nil)
		     ((= (abs (- k (cdadr positions)))
			 (abs (- (caar positions)
				 (caadr positions))))   ;; same diagonal of 1st and 2nd - unsafe
		      nil)
		     (t (safep k (cons (car positions)  ;; check for 1st vs (rest - 2nd)
				       (cddr positions)))))))
    (queen-cols board-size))))
