;;; Exercises 2.79-2.80

(load "SICP-utils.lisp")
(in-package :sicp-utils)

(install-complex-package)

;; Exercise 2.79
(defun equ? (x y)
  (apply-generic 'equ? x y))

;; Exercise 2.80
(defun iszero? (x)
  (apply-generic 'iszero? x))

;; Exercise 2.83
(defun raise (x)
  (apply-generic 'raise x))

(print (raise (raise (make-lisp-integer 5))))
