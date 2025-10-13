;; (use-package :low-crit)

(deftype ordering () '(member :lt :eq :gt))

(declaim (ftype (function (ordering) ordering) reverse-ordering))
(defun reverse-ordering (o)
  (declare (optimize speed))
  (case o
    (:lt :gt)
    (:eq :eq)
    (t :lt)))

(declaim (ftype (function (fixnum) (integer -1 1)) reverse-iord))
(defun reverse-iord (o)
  (declare (optimize speed))
  (cond ((< o 0) 1)
	((= o 0) 0)
	(t -1)))

(defun ord-from-int (i)
  (cond ((< i 0) :lt)
	((= i 0) :eq)
	(t :gt)))

(defun benchmark (size)
  (let ((ords (make-array size))
	(ints (make-array size)))
    (dotimes (i size)
      (let ((iord (- (random 3) 1)))
	(setf (aref ints i) iord)
	(setf (aref ords i) (ord-from-int iord))))
    (low-crit:run-benchmarks
     (list
      (low-crit:bench "int"
		      (dotimes (i size)
			(declare (optimize speed (safety 0)))
			(setf (aref ints i) (reverse-iord (aref ints i)))))
      (low-crit:bench "ord"
		      (dotimes (i size)
			(declare (optimize speed (safety 0)))
			(setf (aref ords i) (reverse-ordering (aref ords i)))))
      (low-crit:bench "int-inline"
		      (dotimes (i size)
			(declare (optimize speed (safety 0)))
			(setf (aref ints i)
			      (let ((r (aref ints i)))
				(declare (type fixnum r))
				(cond ((< r 0) 1)
				      ((= r 0) 0)
				      (t -1))))))
      (low-crit:bench "ord-inline"
		      (dotimes (i size)
			(declare (optimize speed (safety 0)))
			(setf (aref ords i)
			      (case (aref ords i)
				(:lt :gt)
				(:eq :eq)
				(t :lt))))))
     :verbose t)))
      
