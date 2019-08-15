;;; Exercises 1.18 & 1.19

(load "SICP-utils")
(use-package :sicp-utils)

(defun repeated-action (arg nrep combiner doubler)
  (labels ((iter-helper (arg acc nrep)
	     (cond ((zerop nrep) acc)
		   ((evenp nrep) (iter-helper (funcall doubler arg) acc (ash nrep -1)))
		   (t (iter-helper arg (funcall combiner arg acc) (1- nrep))))))
    (iter-helper arg (funcall combiner) nrep)))

(defun fast-expt (b n)
  (repeated-action b n '* #'square))

(defun fast-mult (a b)
  (repeated-action a b '+ #'(lambda (x) (ash x 1))))

(defun fast-fib (n)
  (labels ((tdouble (pq)
	     (funcall
	      #'(lambda (x y z)
		  (list (+ x y) (+ y (* 2 z))))
	      (square (car pq))
	      (square (cadr pq))
	      (* (car pq) (cadr pq))))

	   (t-mul-ab (&optional tpq ab)
	     (if (null ab)
		 (list 1 0)
		 (list (+ (* (cadr tpq) (+ (car ab) (cadr ab))) (* (car tpq) (car ab)))
		       (+ (* (car tpq) (cadr ab)) (* (cadr tpq) (car ab)))))))
    (cadr (repeated-action (list 0 1) n #'t-mul-ab #'tdouble))))
