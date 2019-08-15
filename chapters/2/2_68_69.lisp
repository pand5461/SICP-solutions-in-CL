;;; Exercises 2.68, 2.69

;; basic definitions from chapter 2.3.4

(defun make-leaf (sym w) (list 'leaf sym w))
(defun leaf? (obj) (eq (car obj) 'leaf))
(defun symbol-leaf (x) (cadr x))
(defun weight-leaf (x) (caddr x))

(defun make-code-tree (lbranch rbranch)
  (list lbranch
	rbranch
	(append (symbols lbranch) (symbols rbranch))
	(+ (weight lbranch) (weight rbranch))))

(defun lbranch (tree) (car tree))
(defun rbranch (tree) (cadr tree))
(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
	     (if (null bits)
		 ()
		 (let ((next-branch
			(choose-branch (car bits) current-branch)))
		   (if (leaf? next-branch)
		       (cons (symbol-leaf next-branch)
			     (decode-1 (cdr bits) tree))
		       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (lbranch branch))
	((= bit 1) (rbranch branch))
	(t (error "bad bit ~S: CHOOSE-BRANCH" bit))))

(defun set-adjoin (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(t (cons (car set) (set-adjoin x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      ()
      (let ((pair (car pairs)))
	(set-adjoin (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;; 2.68
(defun encode (message tree)
  (if (null message)
      ()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(defun encode-symbol (sym tree)
  (cond ((leaf? tree) nil)
	((member sym (symbols (lbranch tree)))
	 (cons 0 (encode-symbol sym (lbranch tree))))
	((member sym (symbols (rbranch tree)))
	 (cons 1 (encode-symbol sym (rbranch tree))))
	(t (error "symbol ~S is not present in the encode tree: ENCODE-SYMBOL" sym))))

;; 2.69
(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(defun successive-merge (leaf-set)
  (cond ((null (cdr leaf-set)) (car leaf-set))
	(t (successive-merge (set-adjoin (make-code-tree (car leaf-set)
							 (cadr leaf-set))
					 (cddr leaf-set))))))

;; 2.70
(let* ((rock-song-lyrics
	 '((A 2) (GET 2) (SHA 3) (WAH 1)
	   (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
       (rock-huffman-tree
	(generate-huffman-tree rock-song-lyrics))
       (message
	'(Get a job
	  Sha na na na na na na na na
	  Get a job
	  Sha na na na na na na na na
	  Wah yip yip yip yip yip yip yip yip yip
	  Sha boom))
       (encoded-message
	(encode message rock-huffman-tree)))
  (print encoded-message)
  (format t "~&Initial message length: ~a~%Encoded message length: ~a~%"
	  (length message) ; 
	  (length encoded-message)) ; prints 84
  ;; ensure that decoding is right
  (print (decode encoded-message rock-huffman-tree)))

;; With a fixed bit length, every symbol
;; takes 3 bits.
;; Overall, 36 * 3 = 108 bits
