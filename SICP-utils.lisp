;;; This package contains the utility functions
;;; defined in the textbook section of SICP (not in exercises)
(defpackage :sicp
  (:use :cl)
  (:export :put-op
	   :get-op
	   :hypot
	   :attach-tag
	   :type-tag
	   :contents
	   :apply-generic
	   :*op-table*
	   :add
	   :sub
	   :mul
	   :div
	   :install-lisp-integer-package
	   :make-lisp-integer
	   :install-rational-package
	   :make-rational
	   :install-rectangular-package
	   :install-polar-package
	   :real-part
	   :imag-part
	   :magnitude
	   :angle
	   :make-from-real-imag
	   :make-from-mag-ang
	   :install-complex-package
	   :make-complex-from-real-imag
	   :make-complex-from-mag-ang))

(in-package :sicp)

(defun hypot (a b)
  (let ((aa (coerce (abs a) 'double-float))
	(ab (coerce (abs b) 'double-float)))
    (cond ((> aa ab)
	   (* aa (sqrt (+ 1d0 (square (/ ab aa))))))
	  ((zerop ab) 0d0)
	  (t
	   (* ab (sqrt (+ 1d0 (square (/ aa ab)))))))))

;; tag system for Chapters 2.4-2.5
;; native integer optimization
;; according to Exercise 2.78
(defun attach-tag (type-tag contents)
  (if (or (eq type-tag 'lisp-integer)
          (eq type-tag 'lisp-float))
      contents
      (cons type-tag contents)))

(defun type-tag (datum)
  (cond ((consp datum)
	 (car datum))
	((integerp datum)
	 'lisp-integer)
        ((typep datum 'float)
         'lisp-float)
	(t (error "Bad tagged datum ~a: TYPE-TAG" datum))))

(defun contents (datum)
  (cond ((consp datum)
	 (cdr datum))
	((integerp datum)
	 datum)
        ((typep datum 'float)
         datum)
	(t (error "BAD tagged datum ~a: CONTENTS" datum))))

(defun apply-generic (op &rest args)
  (let* ((type-tags (mapcar #'type-tag args))
	 (proc (get-op op type-tags)))
    (if proc
        (if (member op '(add
                         sub
                         mul
                         div))
            (drop (apply proc (mapcar #'contents args)))
            (apply proc (mapcar #'contents args)))
	(if (= (length type-tags) 2)
            (cond
              ((derivative-type? (car type-tags)
                                 (cadr type-tags))
               (apply-generic op
                              (car args)
                              (raise (cadr args))))
              ((derivative-type? (cadr type-tags)
                                 (car type-tags))
               (apply-generic op
                              (raise (car args))
                              (cadr args)))
              (t
               (error "No method ~a for types ~a: APPLY-GENERIC"
	              op
	              type-tags)))
            (error "No method ~a for types ~a: APPLY-GENERIC"
	           op
	           type-tags)))))

;; put-op and get-op for Chapter 2.4
(defun make-op-table ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
	       (let ((subtable
		      (assoc key-1 (cdr local-table))))
		 (if subtable
		     (let ((record
			    (assoc key-2 (cdr subtable) :test #'equal)))
		       (if record
			   (cdr record)
			   nil))
		     nil)))
	     
	     (insert! (key-1 key-2 value)
	       (let ((subtable
		      (assoc key-1 (cdr local-table))))
		 (if subtable
		     (let ((record
			    (assoc key-2 (cdr subtable) :test #'equal)))
		       (if record
			   (setf (cdr record) value)
			   (setf (cdr subtable)
				 (cons (cons key-2 value)
				       (cdr subtable)))))
		     (setf (cdr local-table)
			   (cons (list key-1 (cons key-2 value))
				 (cdr local-table)))))
	       'ok)
	     
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc!) #'insert!)
		     (t (error "Unknown operation: ~a TABLE" m)))))
      #'dispatch)))

(defparameter *op-table* (make-op-table))

(defun get-op (op type)
  (funcall (funcall *op-table* 'lookup-proc) op type))

(defun put-op (op type value)
  (funcall (funcall *op-table* 'insert-proc!) op type value))

;; generic arithmetics package for Chapter 2.5
(defun add (x y)
  (apply-generic 'add x y))

(defun sub (x y)
  (apply-generic 'sub x y))

(defun mul (x y)
  (apply-generic 'mul x y))

(defun div (x y)
  (apply-generic 'div x y))

(defun install-lisp-integer-package ()
  (labels ((tag (x)
	     (attach-tag 'lisp-integer
			 x)))
    (unless (lookup-type 'lisp-integer)
      (add-type 'lisp-integer t))
    (put-op 'add
	    '(lisp-integer lisp-integer)
	    #'(lambda (x y) (tag (+ x y))))
    (put-op 'sub
	    '(lisp-integer lisp-integer)
	    #'(lambda (x y) (tag (- x y))))
    (put-op 'mul
	    '(lisp-integer lisp-integer)
	    #'(lambda (x y) (tag (* x y))))
    (put-op 'div
	    '(lisp-integer lisp-integer)
	    #'(lambda (x y) (tag (truncate x y))))
    (put-op 'make 'lisp-integer
	    #'(lambda (x) (tag x)))
    ;; Exercise 2.79
    (put-op 'equ?
	    '(lisp-integer lisp-integer)
	    #'eql)
    ;; Exercise 2.80
    (put-op 'iszero? '(lisp-integer)
	    #'zerop)
    'done))

(defun make-lisp-integer (n)
  (funcall (get-op 'make 'lisp-integer) n))

(defun install-lisp-float-package ()
  (labels ((tag (x)
	     (attach-tag 'lisp-float
			 x)))
    (unless (lookup-type 'rational)
      (install-rational-package))
    (unless (lookup-type 'lisp-float)
      (add-type 'lisp-float 'rational))
    (put-op 'add
	    '(lisp-float lisp-float)
	    #'(lambda (x y) (tag (+ x y))))
    (put-op 'sub
	    '(lisp-float lisp-float)
	    #'(lambda (x y) (tag (- x y))))
    (put-op 'mul
	    '(lisp-float lisp-float)
	    #'(lambda (x y) (tag (* x y))))
    (put-op 'div
	    '(lisp-float lisp-float)
	    #'(lambda (x y) (tag (/ x y))))
    (put-op 'make 'lisp-float
	    #'(lambda (x) (tag x)))
    ;; Exercise 2.79
    (put-op 'equ?
	    '(lisp-float lisp-float)
	    #'=)
    ;; Exercise 2.80
    (put-op 'iszero? '(lisp-float)
	    #'zerop)
    ;;Exercise 2.83
    (put-op 'raise '(rational)
	    (lambda (r)
              (let ((tagged-r (attach-tag
                               'rational
                               r)))
	        (tag
	         (float
                  (/ (numer tagged-r)
		     (denom tagged-r))
                  1d0)))))
    (put-op 'project '(lisp-float)
            (lambda (f)
              (let ((rat-f (rational f)))
                (make-rational
                 (numerator rat-f)
                 (denominator rat-f)))))
    'done))

(defun install-rectangular-package ()
    (labels (
	     ;; internal procedures
	     (real-part (z) (car z))
	     (imag-part (z) (cdr z))
	     (make-from-real-imag (x y) (cons x y))
	     (magnitude (z)
	       (hypot (real-part z) (imag-part z)))
	     (angle (z)
	       (atan (imag-part z) (real-part z)))
	     (make-from-mag-ang (r a)
	       (cons (* r (cos a)) (* r (sin a))))
	     ;; interface to the rest of the system
	     (tag (x) (attach-tag 'rectangular x)))
      (put-op 'real-part '(rectangular) #'real-part)
      (put-op 'imag-part '(rectangular) #'imag-part)
      (put-op 'magnitude '(rectangular) #'magnitude)
      (put-op 'angle '(rectangular) #'angle)
      (put-op 'make-from-real-imag 'rectangular
	   (lambda (x y) (tag (make-from-real-imag x y))))
      (put-op 'make-from-mag-ang 'rectangular
	   (lambda (r a) (tag (make-from-mag-ang r a))))
      'done))

(defun install-polar-package ()
    (labels (
	     ;; internal procedures
	     (magnitude (z) (car z))
	     (angle (z) (cdr z))
	     (make-from-mag-ang (r a) (cons r a))
	     (real-part (z)
	       (* (magnitude z) (cos (angle z))))
	     (imag-part (z)
	       (* (magnitude z) (sin (angle z))))
	     (make-from-real-imag (x y)
	       (cons (hypot x y)
		     (atan y x)))
	     ;; interface to the rest of the system
	     (tag (x) (attach-tag 'polar x)))
      (put-op 'real-part '(polar) #'real-part)
      (put-op 'imag-part '(polar) #'imag-part)
      (put-op 'magnitude '(polar) #'magnitude)
      (put-op 'angle '(polar) #'angle)
      (put-op 'make-from-real-imag 'polar
	   (lambda (x y) (tag (make-from-real-imag x y))))
      (put-op 'make-from-mag-ang 'polar
	   (lambda (r a) (tag (make-from-mag-ang r a))))
      'done))

(defun real-part (z) (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))

(defun make-from-real-imag (x y)
  (funcall (get-op 'make-from-real-imag 'rectangular) x y))

(defun make-from-mag-ang (r a)
  (funcall (get-op 'make-from-mag-ang 'polar) r a))


(defun install-rational-package ()
  (labels (
	   ;; internal procedures
	   (numer (x) (car x))
	   (denom (x) (cdr x))
	   (make-rat (n d)
	     (let* ((g (gcd n d))
		    (rnum (/ (abs n) g))
		    (rden (/ (abs d) g)))
	       (if (= (signum n) (signum d))
		   (cons rnum rden)
		   (cons (- rnum) rden))))
	   (add-rat (x y)
	     (make-rat (+ (* (numer x) (denom y))
			  (* (numer y) (denom x)))
		       (* (denom x) (denom y))))
	   (sub-rat (x y)
	     (make-rat (- (* (numer x) (denom y))
			  (* (numer y) (denom x)))
		       (* (denom x) (denom y))))
	   (mul-rat (x y)
	     (make-rat (* (numer x) (numer y))
		       (* (denom x) (denom y))))
	   (div-rat (x y)
	     (make-rat (* (numer x) (denom y))
		       (* (denom x) (numer y))))
	   ;; interface to the rest of the system
	   (tag (x) (attach-tag 'rational x)))
    (unless (lookup-type 'lisp-integer)
      (install-lisp-integer-package))
    (add-type 'rational 'lisp-integer)
    (put-op 'add '(rational rational)
	    (lambda (x y) (tag (add-rat x y))))
    (put-op 'sub '(rational rational)
	    (lambda (x y) (tag (sub-rat x y))))
    (put-op 'mul '(rational rational)
	    (lambda (x y) (tag (mul-rat x y))))
    (put-op 'div '(rational rational)
	    (lambda (x y) (tag (div-rat x y))))
    (put-op 'make 'rational
	    (lambda (n d) (tag (make-rat n d))))
    (put-op 'numer '(rational)
            (lambda (x) (make-lisp-integer
                         (numer x))))
    (put-op 'denom '(rational)
            (lambda (x) (make-lisp-integer
                         (denom x))))
    (put-op 'numer '(lisp-integer)
            #'make-lisp-integer)
    (put-op 'denom '(lisp-integer)
            (lambda (x) (make-lisp-integer 1)))
    ;; Exercise 2.79
    (put-op 'equ? '(rational rational)
	    (lambda (x y)
	     (or (and (eql (numer x) 0)
		      (eql (numer y) 0))
		 (and (eql (numer x) (numer y))
		      (eql (denom x) (denom y))))))
    ;; Exercise 2.80
    (put-op 'iszero? '(rational)
	    (lambda (x) (zerop (numer x))))
    ;; Exercise 2.83
    (put-op 'raise '(lisp-integer)
	    (lambda (x)
	      (make-rational x 1)))
    ;; Exercise 2.85
    (put-op 'project '(rational)
            (lambda (r)
              (make-lisp-integer
               (round (numer r)
                      (denom r)))))
    'done))

(defun make-rational (n d)
  (funcall (get-op 'make 'rational) n d))

(defun numer (r)
  (apply-generic 'numer r))

(defun denom (r)
  (apply-generic 'denom r))

(defun install-complex-package ()
  (labels (
	   ;; from rectangular and polar packages
	   (make-from-real-imag (x y)
	     (funcall (get-op 'make-from-real-imag 'rectangular) x y))
	   (make-from-mag-ang (r a)
	     (funcall (get-op 'make-from-mag-ang 'polar) r a))
	   ;; internal procedures
	   (add-complex (z1 z2)
	     (make-from-real-imag
	      (+ (real-part z1) (real-part z2))
	      (+ (imag-part z1) (imag-part z2))))
	   (sub-complex (z1 z2)
	     (make-from-real-imag
	      (- (real-part z1) (real-part z2))
	      (- (imag-part z1) (imag-part z2))))
	   (mul-complex (z1 z2)
	     (make-from-mag-ang
	      (* (magnitude z1) (magnitude z2))
	      (+ (angle z1) (angle z2))))
	   (div-complex (z1 z2)
	     (make-from-mag-ang
	      (/ (magnitude z1) (magnitude z2))
	      (- (angle z1) (angle z2))))
	   ;; interface to the rest of the system
	   (tag (z) (attach-tag 'complex z)))
    (unless (lookup-type 'lisp-float)
      (install-lisp-float-package))
    (unless (lookup-type 'complex)
      (install-rectangular-package)
      (install-polar-package)
      (add-type 'complex 'lisp-float))
    (put-op 'add '(complex complex)
	    (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put-op 'sub '(complex complex)
	    (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put-op 'mul '(complex complex)
	    (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put-op 'div '(complex complex)
	    (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put-op 'make-from-real-imag 'complex
	    (lambda (x y) (tag (make-from-real-imag x y))))
    (put-op 'make-from-mag-ang 'complex
	    (lambda (r a) (tag (make-from-mag-ang r a))))
    ;; Exercise 2.77
    (put-op 'real-part '(complex) #'real-part)
    (put-op 'imag-part '(complex) #'imag-part)
    (put-op 'magnitude '(complex) #'magnitude)
    (put-op 'angle '(complex) #'angle)
    ;; Exercise 2.79
    (put-op 'equ? '(complex complex)
	    (lambda (z1 z2)
	      (and (= (real-part z1) (real-part z2))
		   (= (imag-part z1) (imag-part z2)))))
    ;; Exercise 2.80
    (put-op 'iszero? '(complex)
	    (lambda (z) (zerop (magnitude z))))
    ;; Exercise 2.83
    (put-op 'raise '(lisp-float)
            (lambda (x) (tag (make-from-real-imag x 0d0))))
    ;; Exercise 2.85
    (put-op 'project '(complex) #'real-part)
    'done))

(defun make-complex-from-real-imag (x y)
  (funcall (get-op 'make-from-real-imag 'complex) x y))

(defun make-complex-from-mag-ang (r a)
  (funcall (get-op 'make-from-mag-ang 'complex) r a))

;; Exercises 2.83-2.85
(defun make-type-tower ()
    (let ((local-table (list '*table*)))
      (labels ((insert-type! (new-type parent-type)
                 (if (eq parent-type t)
                     (setf (cdr local-table)
                           (cons (list new-type)
                                 (cdr local-table)))
                     (let ((subtable (lookup-type parent-type
                                                  (cdr local-table))))
                       (when (null subtable)
                         (error "Parent type ~a not found: ADD-TYPE" parent-type))
                       (when (cdr subtable)
                         (error "Parent type ~a already has a descendant" parent-type))
                       (setf (cdr subtable)
                             (cons (list new-type)
                                   (cdr subtable)))))
                 (print local-table)
                 'ok)

               (lookup-type (test-type type-tower)
                 (cond ((null type-tower)
                        nil)
                       ((eq (caar type-tower)
                            test-type)
                        (car type-tower))
                       (t (lookup-type test-type
                                       (cdar type-tower)))))

               (dispatch (m)
                 (cond ((eq m 'insert-type!)
                        #'insert-type!)
                       ((eq m 'lookup-type)
                        #'(lambda (test)
                            (lookup-type test (cdr local-table))))
                       (t
                        (error "Unknown operation ~a: TYPE_TOWER" m)))))
        #'dispatch)))

(defparameter *number-types* (make-type-tower))

(defun add-type (new-type &optional (parent-type t))
  (funcall (funcall *number-types* 'insert-type!) new-type parent-type))

(defun lookup-type (test-type)
  (funcall (funcall *number-types* 'lookup-type) test-type))

(defun derivative-type? (test-type parent-type)
  (labels ((rmember (item tower)
             (cond ((null tower)
                    nil)
                   ((eq item
                        (caar tower))
                    (car tower))
                   (t
                    (rmember item
                             (cdar tower))))))
              
    (rmember test-type (cdr (lookup-type parent-type)))))

(defun drop (x)
  (let* ((xtype (type-tag x))
         (proj (get-op 'project
                       (list xtype))))
    (if proj
        (let ((xproj (apply proj (list (contents x)))))
          (if (equ? xproj x)
              (drop xproj)
              x))
        x)))
