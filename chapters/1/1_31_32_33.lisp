;;; Exercises 1.31 - 1.33

(load "SICP-utils")
(use-package :sicp-utils)

;; tail-recursive definition of #'accumulate for 1.32
(defun accumulate (combiner null-value term a next b)
  (labels ((acc-iter (a curr-val)
	     (if (> a b)
		 curr-val
		 (acc-iter (funcall next a)
			   (funcall combiner
				    curr-val
				    (funcall term a))))))
    (acc-iter a null-value)))

;; #'sum and #'product in terms of #'accumulate (1.32 and 1.31)
(defun sum (term a next b)
  (accumulate #'+ 0 term a next b))

(defun product (term a next b)
  (accumulate #'* 1 term a next b))

;; #'filered-accumulate for 1.33
(defun filtered-accumulate (combiner null-value test term a next b)
  (labels ((acc-iter (a curr-val)
	     (cond ((> a b) curr-val)
		   ((funcall test a) (acc-iter (funcall next a)
					       (funcall combiner
							curr-val
							(funcall term a))))
		   (t (acc-iter (funcall next a)
				curr-val)))))
    (acc-iter a null-value)))

;; sum of squares of prime numbers for 1.33
(defun prime-p (n)
  (cond ((<= n 1) nil)
	(t(= n (smallest-divisor n)))))

(defun smallest-divisor (n)
  (labels ((next-divisor (divisor)
	     (if (oddp divisor)
		 (+ divisor 2)
		 (1+ divisor)))
	   (find-divisor (test-divisor)
	     (cond ((> (square test-divisor) n) n)
		   ((zerop (rem n test-divisor)) test-divisor)
		   (t (find-divisor (next-divisor test-divisor))))))
    (find-divisor 2)))

(defun sum-prime-squares (a b)
  (filtered-accumulate #'+ 0 #'prime-p #'square a #'1+ b))

;; 2^2 + 3^2 + 5^2 + 7^2 = 87
(format t "Sum of squares of prime numbers from 1 to 10 is ~a~%"
	(sum-prime-squares 1 10))
