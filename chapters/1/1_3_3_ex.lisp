(let ((prev-file "./1_3_3_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.35

(defun ex-1-35 ()
  (format t "Approximation to the golden section as a fixed point: ~a~%"
          (fixed-point (lambda (x) (1+ (/ 1.0d0 x))) 1.0d0)))

;; Exercise 1.36

(defun verbose-fixed-point (f first-guess)
  (let ((rtol 1d-6))
    (labels
        ((close-enough? (v1 v2)
           (< (abs (- v1 v2)) (* rtol (average v1 v2))))
         (try (guess ntry)
           (let ((next (funcall f guess)))
             (format t "Approximation #~a is: ~a~%" ntry next)
             (cond ((close-enough? guess next)
                    (format t "Approximation accepted~%")
                    next)
                   (t (try next (1+ ntry)))))))
      (format t "Starting guess: ~a~%" first-guess)
      (try (* 1d0 first-guess) 1))))

(defun ex-1-36 ()
  (format t "Solving x^x = 1000 by fixed point x = log(1000) / log(x)~%")
  (format t "Without average damping:~%")
  (verbose-fixed-point #'(lambda (x) (/ (log 1d3) (log x))) 2)
  (format t "With average damping:~%")
  (verbose-fixed-point #'(lambda (x)
                           (average x
                                    (/ (log 1d3) (log x))))
                       2))

;; Exercise 1.37
;; Continued fraction evaluation is controlled by relative tolerance
;; Uses modified Lentz's method (W.H. Press et al., Numerical Recipes)
;; A and B in #'iterator are C_i and D_i from NR
;; N and D are a_i and b_i from NR
(defun cont-frac (n d rtol)
  (let ((tiny 1d-30)) 
    (labels
        ((limit-abs (x)
           (if (= x 0)
               tiny
               x))
         (next-a (a index)
           (limit-abs (+ (funcall d index)
                         (/ (funcall n index)
                            a))))
         (next-b (b index)
           (/ 1.0d0 (limit-abs (+ (funcall d index)
                                  (* (funcall n index)
                                     b)))))
         (iterator (ans a b index)
           (let* ((an (next-a a index))
                  (bn (next-b b index))
                  (delta (* an bn)))
             (format t "Fraction depth: ~a~%" index)
             (if (< (abs (- delta 1.0d0)) rtol)
                 (* ans delta)
                 (iterator (* ans delta)
                           an
                           bn
                           (1+ index))))))
      (let ((f0 (limit-abs (* 1d0 (funcall d 0)))))
	(iterator f0 f0 0 1)))))

(defun ex-1-37 ()
  (format t "Calculating the golden ratio...~%")
  (format t "Golden ratio approximation to 4 decimal digits is ~,4e~%"
          (cont-frac #'(lambda (i) 1d0)
                     #'(lambda (i) (if (= i 0) 0 1d0))
                     1d-4)))

;; Exercise 1.38

(defun ex-1-38 ()
  (format t "Calculating the Euler's number...~%")
  (format t "Euler's number approximation to 4 decimal digits is ~,4e~%"
          (cont-frac #'(lambda (i) 1d0)
                     #'(lambda (i) (if (= i 0)
                                       2
                                       (multiple-value-bind (fr rem) (truncate i 3)
                                         (if (= rem 2)
                                             (* (1+ fr) 2)
                                             1))))
                     1d-4)))

;; Exercise 1.39

(defun tan-cf (x)
  (cont-frac #'(lambda (i) (if (> i 1) (- (* x x)) x))
	     #'(lambda (i) (if (= i 0) 0 (+ i i -1)))
	     1d-12))
