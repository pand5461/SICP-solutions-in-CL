(let ((prev-file "./1_2_6_textbook.lisp"))
  (load prev-file))

(in-package :sicp)

(defun cube (x) (* x x x))

(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
         (sum term (funcall next a) next b))))

(defun integral-midpoint (f a b dx)
  (labels ((add-dx (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2.0d0)) #'add-dx b)
       dx)))

(export '(cube sum integral-midpoint) :sicp)
