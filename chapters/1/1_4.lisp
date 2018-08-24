;;; Exercise 1.4

(defun a-plus-abs-b (a b)
  "Returns a + abs(b)"
  (funcall (if (> b 0) #'+ #'-) a b))
