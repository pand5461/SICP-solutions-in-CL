;;; Exercises 2.56 - 2.58

;; basic definitions from chapter 2.3.2

(defun var? (x) (symbolp x))

(defun same-var? (v1 v2)
  (and (var? v1) (var? v2) (eq v1 v2)))

(defun make-sum (a1 a2)
  (cond ((num= a1 0) a2)
	((num= a2 0) a1)
	((and (numberp a1)
	      (numberp a2))
	 (+ a1 a2))
	(t (list '+ a1 a2))))

(defun num= (expr num)
  (and (numberp expr) (= expr num)))

(defun make-product (m1 m2)
  (cond ((num= m1 0) 0)
	((num= m2 0) 0)
	((num= m1 1) m2)
	((num= m2 1) m1)
	((and (numberp m1)
	      (numberp m2))
	 (* m1 m2))
	(t (list '* m1 m2))))

(defun sum? (expr)
  (and (consp expr) (eq (car expr) '+)))
(defun add1 (s) (cadr s))
(defun add2 (s) (caddr s))

(defun product? (expr)
  (and (consp expr) (eq (car expr) '*)))
(defun mult1 (p) (cadr p))
(defun mult2 (p) (caddr p))

;; 2.56

(defun deriv (expr var)
  (cond ((numberp expr) 0)
	((var? expr) (if (same-var? expr var) 1 0))
	((sum? expr) (make-sum (deriv (add1 expr) var)
			       (deriv (add2 expr) var)))
	((product? expr)
	 (make-sum
	  (make-product (mult1 expr)
			(deriv (mult2 expr) var))
	  (make-product (mult2 expr)
			(deriv (mult1 expr) var))))
	((expt? expr)
	 (make-product (power expr)
		       (make-product (make-expt (base expr) (1- (power expr)))
				     (deriv (base expr) var))))
	(t (error "unknown expression type: (DERIV ~S)" expr))))

(defun expt? (expr)
  (and (consp expr) (eq (car expr) 'expt)))
(defun base (expr) (cadr expr))
(defun power (expr) (caddr expr))

(defun make-expt (base power)
  (cond ((and (num= base 0) (numberp power) (<= power 0))
	 (error "cannot raise 0 to non-positive power: DERIV"))
	((num= base 0) 0)
	((num= power 0) 1)
	((num= power 1) base)
	(t (list 'expt base power))))

;; 2.57

(defun add2 (s)
  (if (null (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))
(defun mult2 (p)
  (if (null (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;; 2.58a

;; (defun make-sum (a1 a2)
;;   (cond ((num= a1 0) a2)
;; 	((num= a2 0) a1)
;; 	((and (numberp a1)
;; 	      (numberp a2))
;; 	 (+ a1 a2))
;; 	(t (list a1 '+ a2))))

;; (defun make-product (m1 m2)
;;   (cond ((num= m1 0) 0)
;; 	((num= m2 0) 0)
;; 	((num= m1 1) m2)
;; 	((num= m2 1) m1)
;; 	((and (numberp m1)
;; 	      (numberp m2))
;; 	 (* m1 m2))
;; 	(t (list m1 '* m2))))

;; (defun sum? (expr)
;;   (and (consp expr) (eq (cadr expr) '+)))
;; (defun add1 (s) (car s))
;; (defun add2 (s) (caddr s))

;; (defun product? (expr)
;;   (and (consp expr) (eq (cadr expr) '*)))
;; (defun mult1 (p) (car p))
;; (defun mult2 (p) (caddr p))

;; 2.58b

;; if any list member is '+, then it is a sum
;; (defun sum? (expr)
;;   (cond ((symbolp expr) nil)
;; 	(t
;; 	 (let ((operand1 (car expr))
;; 	       (fn (cadr expr))
;; 	       (operand2 (cddr expr)))
;; 	   (cond ((eq fn '+) t)
;; 		 ((symbolp operand2) nil)
;; 		 (t (sum? operand2)))))))

;; search for the last '+ in the expression, everything in front of it is the addend
;; (defun add1 (s)
;;   (if (eq (cadr s) '+)
;;       (car s)
;;       (append (list (car s) (cadr s))
;; 	      (list (add1 (cddr s))))))

;; search for the last '+ in the expression, everything after it is the augend
;; (defun add2 (s)
;;   (if (eq (cadr s) '+)
;;       (if (null (cdddr s))
;; 	  (caddr s)
;; 	  (cddr s))
;;       (add2 (cddr s))))

;; if an expression is not a sum, and the infix operator is '*, then it is a product
;; selectors remain the same as in 2.58a
;; (defun product? (expr)
;;   (and (consp expr) (not (sum? expr)) (eq (cadr expr) '*)))
