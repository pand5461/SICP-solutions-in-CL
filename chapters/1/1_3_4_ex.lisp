(let ((prev-file "./1_3_4_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.40

(defun cubic (a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;;; Exercise 1.41
(defun apply-twice (f)
  (lambda (x)
    (funcall f (funcall f x))))

(defun ex-1-41 ()
  (format t "~a~%"
          (funcall (funcall (apply-twice (apply-twice #'apply-twice)) #'1+) 5)))

;;; Exercise 1.42
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;;; Exercise 1.43
(defun repeated (f times)
  (if (= times 0)
      #'identity
      (compose f (repeated f (1- times)))))

;;; Exercise 1.44
(defun smooth (f)
  (let ((dx 0.01d0))
    (lambda (x) (average (funcall f (+ x dx))
                         (funcall f x)
                         (funcall f (- x dx))))))

(defun nfold-smooth (f n)
  (funcall (repeated #'smooth n) f))

;;; Exercise 1.45

;; Experiments show that one needs log₂n average dampings
;; to achieve convergence with n-th roots
(defun nth-root (x n)
  (let ((damp-count (floor (log n)
                           (log 2))))
    (fixed-point (funcall (repeated #'average-damp
                                    damp-count)
                          (lambda (y) (/ x (expt y (1- n)))))
                 1.0d0)))

;;; Exercise 1.46

(defun iterative-improve (validate improve)
  (labels
      ((iter (guess)
         (let ((next-guess (funcall improve guess)))
           (if (funcall validate guess next-guess)
               next-guess
               (iter next-guess)))))
    #'iter))

(defun sqrt2 (x)
  (funcall (iterative-improve
            (lambda (y z)
              (let ((rtol 1.0d-5))
                (< (abs (- y z))
                   (* rtol (average y z)))))
            (lambda (y) (average y (/ x y))))
           1.0d0))

(defun fixed-point2 (f first-guess)
  (funcall (iterative-improve
            (lambda (y z)
              (let ((rtol 1.0d-5))
                (< (abs (- y z))
                   (* rtol (average y z)))))
            f)
           first-guess))
