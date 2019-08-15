;;; Exercise 2.75

(load "SICP-utils")
(use-package :sicp-utils)

;; basic definitions from chapter 2.4

(defun make-from-real-imag (x y)
  (labels ((dispatch (op)
	     (cond ((eq op 'real-part) x)
		   ((eq op 'imag-part) y)
		   ((eq op 'magnitude)
		    (hypot x y))
		   ((eq op 'angle)
		    (atan (coerce y 'double-float)
			  (coerce x 'double-float)))
		   (t
		    (error "Unknown operation ~a: MAKE-FROM-REAL-IMAG" op)))))
    #'dispatch))

(defun apply-generic (op object &rest rest-args)
  (apply object (cons op rest-args)))

;; Exercise 2.75
(defun make-from-mag-ang (r a)
  (labels ((dispatch (op)
	     (cond ((eq op 'real-part)
		    (* r (cos (coerce a 'double-float))))
		   ((eq op 'real-part)
		    (* r (sin (coerce a 'double-float))))
		   ((eq op 'magnitude) r)
		   ((eq op 'angle) a)
		   (t
		    (error "Unknown operation ~a: MAKE-FROM-MAG-ANG" op)))))
    #'dispatch))   
