(let ((prev-file "./1_2_6_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;; Exercise 1.21
(defun print-smallest-divisor (n)
  (format t "Smallest divisor of ~a is ~a~%" n (smallest-divisor n)))

(defun ex-1-21 ()
  (print-smallest-divisor   199)   ;; 199
  (print-smallest-divisor  1999)   ;; 1999
  (print-smallest-divisor 19999))  ;; 7

;;; Exercise 1.22
(defun timed-prime-test (n)
  (labels
      ((report-prime (n elapsed-time-ms)
         (format t "~a *** ~a ms~%" n elapsed-time-ms)
         t)
       (start-prime-test (n start-time)
         (if (prime-p n)
             (report-prime n (- (get-internal-real-time) start-time))
             nil)))
    (start-prime-test n (get-internal-real-time))))

(defun timed-prime-search (nstart nprimes)
  (labels
      ((next-n (n)
         (if (oddp n)
             (+ n 2)
             (+ n 1))))
    (cond ((zerop nprimes) nil)
	  ((= nstart 1) (timed-prime-search 2 nprimes))
	  ((timed-prime-test nstart) (timed-prime-search (next-n nstart) (1- nprimes)))
	  (t (timed-prime-search (next-n nstart) nprimes)))))

;; 1.22: Testing if x10 increase in test prime number increases time to test by sqrt(10) ~ 3 times

(defun ex-1-22 ()
  (timed-prime-search 10000000000000   3)  ;; times around 70 on my machine
  (timed-prime-search 100000000000000  3)  ;; around 215
  (timed-prime-search 1000000000000000 3)) ;; around 685
;; Conclusion: sqrt(n) estimate of complexity is reasonable


;; Exercise 1.23

;; makes sense to include that improvement directly into the base functions
(defun faster-find-divisor (n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (t (faster-find-divisor n (next-divisor test-divisor)))))

(defun next-divisor (m)
  (if (= m 2)
      3
      (+ m 2)))

(defun faster-prime-p (n)
  (= n (faster-find-divisor n 2)))

(defun timed-faster-prime-test (n)
  (labels
      ((report-prime (n elapsed-time-ms)
         (format t "~a *** ~a ms~%" n elapsed-time-ms)
         t)
       (start-prime-test (n start-time)
         (if (faster-prime-p n)
             (report-prime n (- (get-internal-real-time) start-time))
             nil)))
    (start-prime-test n (get-internal-real-time))))

(defun ex-1-23 ()
  (format t "Original prime test:~%")
  (timed-prime-test 1000000000000037)
  (format t "Optimized prime test:~%")
  (timed-faster-prime-test 1000000000000037))

;; Exercise 1.24

(defun timed-fast-prime-test (n)
  (labels
      ((report-prime (n elapsed-time-ms)
         (format t "~a *** ~a ms~%" n elapsed-time-ms)
         t)
       (start-prime-test (n start-time)
         (if (fast-prime-p n 100000)
             (report-prime n (- (get-internal-real-time) start-time))
             nil)))
    (start-prime-test n (get-internal-real-time))))

(defun ex-1-24 ()
  (timed-fast-prime-test 10000000000037)
  (timed-fast-prime-test 10000000000051)
  (timed-fast-prime-test 10000000000099)
  (timed-fast-prime-test 100000000000031)
  (timed-fast-prime-test 100000000000067)
  (timed-fast-prime-test 100000000000097)
  (timed-fast-prime-test 1000000000000037)
  (timed-fast-prime-test 1000000000000091)
  (timed-fast-prime-test 1000000000000159))

;; in short - O(log n) hypothesis is more or less confirmed

;;; Exercise 1.28

(defun expmod-sqrt1-check (base expt m)
  (labels
      ((sqrt1-checker (x)
         (cond ((= x 1) 1)
               ((= x (1- m)) 1)
               (t (return-if-not-1 (helper x)))))
       (helper (x)
         (rem (square x) m))
       (return-if-not-1 (x)
         (if (= x 1) 0 x)))
    (cond
      ((zerop expt) 1)
      ((evenp expt)
       (sqrt1-checker (expmod-sqrt1-check base
                                          (/ expt 2)
                                          m)))
      (t
       (rem (* base
               (expmod-sqrt1-check base
                                   (1- expt)
                                   m))
            m)))))

(defun miller-rabin-test (n)
  (labels ((try-it (a)
	     (= (expmod-sqrt1-check a
                                    (1- n)
                                    n)
                1)))
    (try-it (1+ (random (1- n))))))

(defun miller-rabin-prime-p (n)
  (labels ((helper (k)
	     (cond ((= n 1) nil)
		   ((zerop k) t)
		   ((miller-rabin-test n)
                    (helper (truncate k
                                      (sqrt 2))))
		   (t nil))))
    (helper n)))
