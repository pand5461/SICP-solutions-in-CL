;;; Exercises 2.79-2.80

(load "SICP-utils")
(in-package :sicp-utils)

(install-lisp-integer-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; Exercise 2.79
(defun equ? (x y)
  (apply-generic 'equ? x y))

;; Exercise 2.80
(defun iszero? (x)
  (apply-generic 'iszero? x))
