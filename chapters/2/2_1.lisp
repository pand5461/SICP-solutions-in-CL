;;; Exercise 2.1

;; Sign normalization is done by dividing both numerator and denominator
;; by the sign of denominator
(defun make-rat (n d)
  (let ((g (gcd n d))
	(s (signum d)))
    (cons (/ n g s) (/ d g s))))

(defun numer (ratio)
  (car ratio))

(defun denom (ratio)
  (cdr ratio))

(defun print-rat (ratio)
  (princ (numer ratio))
  (princ #\/)
  (princ (denom ratio))
  (princ #\Newline))

(defun rat= (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defun rat+ (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun rat- (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun rat* (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun rat/ (x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))
