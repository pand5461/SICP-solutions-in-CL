(let ((prev-file "./1_1_8_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.16

(defun fast-expt (b n)
  "Raises b to the power n"
  (labels
      ((expt-iter (acc mult n)
         "Returns acc * mult^n"
         (cond ((zerop n) acc)
               ((evenp n) (expt-iter acc (square mult) (/ n 2)))
               (t (expt-iter (* acc mult) mult (1- n))))))
    (expt-iter 1 b n)))

;;; Exercises 1.17 && 1.18

(defun peasant-mul (a b)
  "Multiplies a and b"
  (labels
      ((mul-iter (acc a b)
         "Returns acc + a * b"
         (cond ((zerop b) acc)
               ((evenp b) (mul-iter acc (twice a) (halve b)))
               (t (mul-iter (+ acc a) a (1- b))))))
    (mul-iter 0 a b)))

(defun twice (k) (+ k k))

(defun halve (m) (floor m 2))

;;; Exercise 1.19

(defun fast-fib (n)
  (labels
      ((fib-iter (a b p q count)
         (cond
           ((= count 0) a)
           ((evenp count)
            (fib-iter a
                      b
                      (sum-of-squares p q)
                      (* q (+ q (twice p)))
                      (halve count)))
           (t (fib-iter (+ (* a p) (* b q))
                        (+ (* (+ a b) q) (* b p))
                        p
                        q
                        (1- count))))))
    (fib-iter 0 1 0 1 n)))
