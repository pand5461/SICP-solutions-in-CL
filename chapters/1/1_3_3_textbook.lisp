(let ((prev-file "./1_3_1_textbook.lisp"))
  (load prev-file))

(in-package :sicp)

(defun fixed-point (f first-guess)
  (let ((tol 1d-6))
    (labels
        ((close-enough? (v1 v2)
           (< (abs (- v1 v2)) tol))
         (try (guess)
           (let ((next (funcall f guess)))
             (if (close-enough? guess next)
                 next
                 (try next)))))
      (try (* 1d0 first-guess)))))

(export '(fixed-point) :sicp)
