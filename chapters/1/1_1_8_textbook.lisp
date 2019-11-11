(let ((prev-file "./1_1_7_textbook.lisp"))
  (load prev-file))

(in-package :sicp)

(defun sqrt1 (x)
  "Computes an approximation to the square root of X"
  (labels
      ((good-enough? (guess)
         (< (abs (- (square guess) x)) 0.001d0))
       (improve (guess)
         (average guess (/ x guess)))
       (sqrt-iter (guess)
         (cond
           ((= x 0) 0d0)
           ((good-enough? guess) guess)
           (t (sqrt-iter (improve guess))))))
  (sqrt-iter 1.0d0)))
