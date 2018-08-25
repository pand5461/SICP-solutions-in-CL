;;; Exercises 1.21, 1.22, 1.23

(declaim (inline square))

(defun square (x) (* x x))

(defun prime-p (n)
  (cond ((<= n 1) nil)
	(t(= n (smallest-divisor n)))))

(defun smallest-divisor (n) ;; for 1.23
  (labels ((next-divisor (divisor)
	     (if (oddp divisor)
		 (+ divisor 2)
		 (1+ divisor)))
	   (find-divisor (test-divisor)
	     (cond ((> (square test-divisor) n) n)
		   ((zerop (rem n test-divisor)) test-divisor)
		   (t (find-divisor (next-divisor test-divisor))))))
    (find-divisor 2)))

(defun timed-prime-test (n) ;; for 1.22
  (labels ((print-if (x)
	     (cond (x (format t "~a *** ~a~%" (car x) (cdr x)) t)
		   (t nil)))
	   (start-prime-test (n start-time)
	     (if (prime-p n)
		 (cons n (- (get-internal-real-time) start-time))
		 nil)))
    (print-if (start-prime-test n (get-internal-real-time)))))

(defun timed-prime-search (nstart nprimes) ;; for 1.22
  (labels ((nextn (n)
	     (if (oddp n)
		 (+ n 2)
		 (+ n 1))))
    (cond ((zerop nprimes) nil)
	  ((= nstart 1) (timed-prime-search 2 nprimes))
	  ((timed-prime-test nstart) (timed-prime-search (nextn nstart) (1- nprimes)))
	  (t (timed-prime-search (nextn nstart) nprimes)))))

;; Exercise 1.21
(defun print-smallest-divisor (n)
  (format t "Smallest divisor of ~a is ~a~%" n (smallest-divisor n)))

(print-smallest-divisor 199)    ;; 199
(print-smallest-divisor 1999)   ;; 1999
(print-smallest-divisor 19999)  ;; 7

;; 1.22: Testing if x10 increase in test prime number increases time to test by sqrt(10) ~ 3 times  
(timed-primed-search 10000000000000   3) ;; times around 30 on my machine
(timed-primed-search 100000000000000  3) ;; around 100
(timed-primed-search 1000000000000000 3) ;; around 300
;; Conclusion: sqrt(n) estimate of complexity is reasonable
