(let ((prev-file "./1_1_5_textbook.lisp"))
  (load prev-file))

(use-package :sicp)

;;; Exercise 1.2
(defun ex-1-2 ()
"Evaluates
  5 + 4 + (2 - (3 - (6 + 4/5)))
  -----------------------------
         3(6 - 2)(2 - 7)"
  (/ (+ 5
        4
        (- 2
           (- 3
              (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))

; evaluates to: -37/150

;;; Exercise 1.3
(defun sum-two-largest-squares (a b c)
"Procedure takes three numbers and returns the sum of the squares of the two larger numbers"
  (if (>= a b)
      (+ (square a)
	 (if (>= b c)
	     (square b)
	     (square c)))
      (+ (square b)
	 (if (>= a c)
	     (square a)
	     (square c)))))

;;; Exercise 1.4
(defun a-plus-abs-b (a b)
  "Returns a + abs(b)"
  (funcall (if (> b 0) #'+ #'-) a b))

;;; Exercise 1.5
(defun p () (p))

(defun test (x y)
  (if (= x 0) 0 y))

;; with applicative order, the argument of the function is evaluated first, then the function is applied
;; this causes an error, because definition of #'p has an infinite recursive call
;; with normal order, this must expand to (if (= x 0) 0 (p)) and return 0, because #'if is a special form which does not evaluate the expression in the non-executed branch

(defun ex-1-5 ()
  (test 0 (p)))

;; with the definition (defmacro test (x y) `(if (= ,x 0) 0 ,y)), the above call does return 0,
;; because macro expands into an (if ...) expression
