;;; Exercise 1.5

(defun p () (p))

(defun test (x y)
  (if (= x 0) 0 y))

; with applicative order, the argument of the function is evaluated first, then the function is applied
; this causes an error, because definition of #'p has an infinite recursive call
; with normal order, this must expand to (if (= x 0) 0 (p)) and return 0, because #'if is a special form which does not evaluate the expression in the non-executed branch
(test 0 (p))

; with the definition (defmacro test (x y) `(if (= ,x 0) 0 ,y)), the above call does return 0,
; because macro expands into an (if ...) expression
