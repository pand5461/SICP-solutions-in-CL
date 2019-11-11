(let ((prev-file "./1_3_3_textbook.lisp"))
  (load prev-file))

(in-package :sicp)

(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))

(defun deriv (g)
  (let ((dx 1.0d-5))
    (lambda (x) (/ (- (funcall g (+ x dx))
                      (funcall g x))
                   dx))))

(defun newton-transform (g)
  (lambda (x) (- x (/ (funcall g x)
                      (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(export '(average-damp newtons-method) :sicp)
