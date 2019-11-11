;;; Exercise 1.11

;; Tree-recursive function is fairly obvious
;; Tail-recursive procedure is given below

(defun f (n)
  (labels
      ((fn (fn-1 fn-2 fn-3)
         (+ fn-1
            (* 2 fn-2)
            (* 3 fn-3)))
       (f-iter (n fn-1 fn-2 fn-3)
         (if (zerop n)
             fn-1
             (f-iter (1- n)
                     (fn fn-1 fn-2 fn-3)
                     fn-1
                     fn-2))))
    (if (< n 3)
	n
	(f-iter (- n 3)
                3
                2
                1))))

;;; Exercise 1.12

(defun pascal-triangle (row num)
  (cond
    ((> num row) nil)
    ((= num 1) 1)
    ((= num row) 1)
    (t (+ (pascal-triangle (1- row) num)
          (pascal-triangle (1- row) (1- num))))))
