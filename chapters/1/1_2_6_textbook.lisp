(let ((prev-file "./1_1_8_textbook.lisp"))
  (load prev-file))

(in-package :sicp)

(defun divides? (d n)
  (= (rem n d) 0))
       
(defun find-divisor (n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (t (find-divisor n (1+ test-divisor)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime-p (n)
  (cond ((<= n 1) nil)
	(t (= n (smallest-divisor n)))))

(defun expmod (base expt m)
  (cond
    ((zerop expt) 1)
    ((evenp expt) (rem
                   (square (expmod base (/ expt 2) m))
                   m))
    (t (rem
        (* base (expmod base (1- expt) m))
        m))))

(defun fermat-test (n)
  (labels ((try-it (a)
	     (= (expmod a n n) a)))
    (try-it (1+ (random (1- n))))))

(defun fast-prime-p (n times)
  (cond
    ((= times 0) t)
    ((fermat-test n) (fast-prime-p n (1- times)))
    (t nil)))

(export '(divides? smallest-divisor fast-prime-p prime-p expmod) :sicp)
