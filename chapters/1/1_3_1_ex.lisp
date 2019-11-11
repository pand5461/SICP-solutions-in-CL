(let ((prev-file "./1_3_1_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.29

;; Define Simpson's rule in terms of midpoint rule
(defun int-simpson (fn a b nseg)
  (labels
      ((h () (/ 1.0d0 nseg)))
    (/ (+ (* (h) (+ (funcall fn a) (funcall fn b)))
          (* 4 (integral-midpoint fn a b (h)))
          (* 2 (integral-midpoint fn
                                  (+ a (* (h) 0.5d0))
                                  (- b (* (h) 0.5d0))
                                  (h))))
       6)))

;; Simpson's rule is theoretically exact for polynomial of degree <= 3
(defun ex-1-29 ()
  (format t "Simpson's rule integration of x³ from 0 to 1:~%")
  (format t "N = 10: ~a~%" (int-simpson #'cube 0 1 10))
  (format t "N = 100: ~a~%" (int-simpson #'cube 0 1 100))
  (format t "N = 1000: ~a~%" (int-simpson #'cube 0 1 1000)))

;;; Exercise 1.30

(in-package :sicp)

;; replace the recursive version by the iterative one
(defun sum (term a next b)
  (labels
      ((iter (a result)
         (if (> a b)
             result
             (iter (funcall next a)
                   (+ result
                      (funcall term a))))))
    (iter a 0)))

(in-package :cl-user)

;; Exercise 1.31

(defun product (term a next b)
  (labels
      ((iter (a result)
         (if (> a b)
             result
             (iter (funcall next a)
                   (* result
                      (funcall term a))))))
    (iter a 1)))

(defun pi-fraction (n)
  (labels
      ((numer-term (i)
         (if (evenp i)
             (+ i 2.0d0)
             (+ i 1.0d0)))
       (denom-term (i)
         (if (evenp i)
             (+ i 1.0d0)
             (+ i 2.0d0)))
       (frac-term (i)
         (/ (numer-term i) (denom-term i))))
    (product #'frac-term 1 #'1+ n)))

(defun ex-1-31 ()
  (format t "10 terms: π ≈ ~a~%" (* 4 (pi-fraction 10)))
  (format t "100 terms: π ≈ ~a~%" (* 4 (pi-fraction 100)))
  (format t "1000 terms: π ≈ ~a~%" (* 4 (pi-fraction 1000)))
  (format t "10000 terms: π ≈ ~a~%" (* 4 (pi-fraction 10000))))


;; Exercise 1.32
(defun accumulate (combiner null-value term a next b)
  (labels
      ((acc-iter (a curr-val)
         (if (> a b)
             curr-val
             (acc-iter (funcall next a)
                       (funcall combiner
                                curr-val
                                (funcall term a))))))
    (acc-iter a null-value)))

(defun sum (term a next b)
  (accumulate #'+ 0 term a next b))

(defun product (term a next b)
  (accumulate #'* 1 term a next b))

;; Exercise 1.33
(defun filtered-accumulate (combiner null-value test term a next b)
  (labels
      ((acc-iter (a curr-val)
         (cond ((> a b) curr-val)
               ((funcall test a) (acc-iter (funcall next a)
                                           (funcall combiner
                                                    curr-val
                                                    (funcall term a))))
               (t (acc-iter (funcall next a)
                            curr-val)))))
    (acc-iter a null-value)))

(defun sum-prime-squares (a b)
  (filtered-accumulate #'+ 0 #'prime-p #'square a #'1+ b))

(defun ex-1-33-a ()
  ;; 2^2 + 3^2 + 5^2 + 7^2 = 87
  (format t "Sum of squares of prime numbers from 1 to 10 is ~a~%"
          (sum-prime-squares 1 10)))

(defun sum-rel-primes (n)
  (labels ((rel-prime? (k) (= (gcd k n) 1)))
    (filtered-accumulate #'* 1 #'rel-prime? #'identity 1 #'1+ n)))

(defun ex-1-33-b ()
  ;; 1 * 3 * 7 * 9 = 189
  (format t "Product of positive integers from 1 to 10 relatively prime to 10 is ~a~%"
          (sum-rel-primes 10)))
