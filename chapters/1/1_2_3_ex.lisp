(let ((prev-file "./1_1_8_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.15

(defun p (x) (* x (- 3 (* 4 (square x)))))

(defun sine (angle)
  (if (<= (abs angle) 0.1)
      angle
      (p (sine (/ angle 3.0)))))

(defun ex-1-15 () 
  (trace p)
  (format t "sin(12.15) ≈ ~a~%" (sine 12.15d0))
  (untrace))

;; from the trace, it is seen that #'p is called 5 times
;; overall order of growth O(log a), as log_3(a / 0.1) steps reduce a to a value around ~0.1
