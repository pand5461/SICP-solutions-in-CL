;;; Exercise 1.11

;; Branch-recursive function is fairly obvious
;; Tail-recursive procedure is given below

(defun f (n)
  (labels ((f-iter (n fn-1 fn-2 fn-3)
	     (if (zerop n)
		 fn-1
		 (f-iter (1- n) (+ fn-1 (* 2 fn-2) (* 3 fn-3)) fn-1 fn-2))))
    (if (< n 3)
	n
	(f-iter (- n 3) 3 2 1))))
