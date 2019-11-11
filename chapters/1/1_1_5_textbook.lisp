(let ((prev-file "../../SICP-utils.lisp"))
  (load prev-file))

(in-package :sicp)

(defun square (x) (* x x))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))

(export '(square sum-of-squares) :sicp)
