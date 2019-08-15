;;; Exercise 1.24

(load "SICP-utils")
(use-package :sicp-utils)

(defun expmod (base expt m)
  (cond ((zerop expt) 1)
	((evenp expt)
	 (rem
	  (square (expmod base (/ expt 2) m))
	  m))
	(t
	 (rem
	  (* base (expmod base (1- expt) m))
	  m))))

(defun fermat-test (n)
  (labels ((try-it (a)
	     (= (expmod a n n) a)))
    (try-it (1+ (random (1- n))))))

(defun fast-prime-p (n)
  (labels ((helper (k)
	     (cond ((= n 1) nil)
		   ((zerop k) t)
		   ((fermat-test n) (helper (truncate k 2)))
		   (t nil))))
    (helper n)))

(defun timed-prime-test (n)
  (labels ((print-if (x)
	     (cond (x (format t "~a *** ~a~%" (car x) (cdr x)) t)
		   (t nil)))
	   (start-prime-test (n start-time)
	     (if (fast-prime-p n)
		 (cons n (- (get-internal-real-time) start-time))
		 nil)))
    (print-if (start-prime-test n (get-internal-real-time)))))

(defun timed-prime-search (nstart nprimes)
  (labels ((nextn (n)
	     (if (oddp n)
		 (+ n 2)
		 (+ n 1))))
    (cond ((zerop nprimes) nil)
	  ((= nstart 1) (timed-prime-search 2 nprimes))
	  ((timed-prime-test nstart) (timed-prime-search (nextn nstart) (1- nprimes)))
	  (t (timed-prime-search (nextn nstart) nprimes)))))

;; compare the times for prime testing with the brute-force method
(timed-prime-search 10000000000000 3)
;; now takes negligible time, compared to O(sqrt(n)) method
