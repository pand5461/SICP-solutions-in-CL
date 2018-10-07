;;; Exercise 3.5

(defun monte-carlo (trials experiment)
  (labels ((iter (rem-trials passed-trials)
	     (cond ((zerop rem-trials)
		    (/ passed-trials 1d0 trials))
		   ((funcall experiment)
		    (iter (1- rem-trials)
			  (1+ passed-trials)))
		   (t (iter (1- rem-trials)
			    passed-trials)))))
    (iter trials 0)))

(defun make-point (x y)
  (cons x y))

(defun point-x (p)
  (car p))

(defun point-y (p)
  (cdr p))

(defun make-rectangle (corner diag-corner)
  (let ((lowerleft (make-point
		    (min (point-x corner)
			 (point-x diag-corner))
		    (min (point-y corner)
			 (point-y diag-corner))))
	(upperright (make-point
		     (max (point-x corner)
			  (point-x diag-corner))
		     (max (point-y corner)
			  (point-y diag-corner)))))
    (cons lowerleft upperright)))

(defun rect-lowerleft (r)
  (car r))

(defun rect-upperright (r)
  (cdr r))

(defun rect-area (r)
  (* (- (point-x (rect-upperright r))
	(point-x (rect-lowerleft r)))
     (- (point-y (rect-upperright r))
	(point-y (rect-lowerleft r)))))

(defun estimate-integral (predicate bounding-rectangle ntrials)
  (let ((rarea (rect-area bounding-rectangle))
	(ll (rect-lowerleft bounding-rectangle))
	(ur (rect-upperright bounding-rectangle)))
    (labels ((random-in-range (lo hi)
	       (+ lo (random (- (* 1d0 hi) lo))))
	     (experiment ()
	       (funcall predicate
			(make-point
			 (random-in-range (point-x ll)
					  (point-x ur))
			 (random-in-range (point-y ll)
					  (point-y ur))))))
      (* (monte-carlo ntrials #'experiment) rarea))))

(defun estimate-pi-mc (ntrials)
  (let ((box (make-rectangle (make-point -1 -1)
			     (make-point 1 1))))
    (labels ((sqr (x) (* x x))
	     (inside-circle? (p)
	       (<= (+ (sqr (point-x p))
		      (sqr (point-y p)))
		   1)))
      (estimate-integral #'inside-circle? box ntrials))))
