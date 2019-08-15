;;; Exercises 2.33 - 2.39

;; translation of #'accumulate into CL
(defun accumulate (op null-elt seq)
  (if (null seq)
      null-elt
      (funcall op
	       (car seq)
	       (accumulate op null-elt (cdr seq)))))

;; 2.33
(defun map-list (op seq)
  (accumulate #'(lambda (x y) (cons (funcall op x)
				    y))
	      nil
	      seq))

(defun my-append (seq1 seq2)
  (accumulate #'cons seq2 seq1))

(defun seq-length (seq)
  (accumulate #'(lambda (x y) (1+ y)) 0 seq))

;; 2.34
(defun horner-eval (x coeffs-sequence)
  (accumulate #'(lambda (this-coeff higher-terms)
		  (+ this-coeff
		     (* x higher-terms)))
	      0
	      coeffs-sequence))

;; 2.35
(defun count-leaves (tree)
  (accumulate #'+
	      0
	      (map-list #'(lambda (x)
			    (if (consp x)
				(count-leaves x)
				1))
			tree)))

;; 2.36
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate op init (map-list #'car seqs))
	    (accumulate-n op init (map-list #'cdr seqs)))))

;; 2.37
(defun dot-product (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar #'(lambda (w) (dot-product v w)) m))

(defun transpose (m)
  (accumulate-n #'cons nil m))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar #'(lambda (v) (matrix-*-vector cols v)) m)))

;; 2.38
(defun fold-right (op init-elt seq)
  (accumulate op init-elt seq))

(defun fold-left (op init-elt seq)
  (labels ((iter (result rest)
	     (if (null rest)
		 result
		 (iter (funcall op result (car rest))
		       (cdr rest)))))
    (iter init-elt seq)))

;; 2.39
(defun rreverse (seq)
  (fold-right #'(lambda (x y) (append y (list x))) nil seq))

(defun lreverse (seq)
  (fold-left #'(lambda (x y) (cons y x)) nil seq))
