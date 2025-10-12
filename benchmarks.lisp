(defpackage :benchmarks
  (:use :cl :interval-tables :low-crit)
  (:export :run-all-benchmarks))

(in-package :benchmarks)

(defun gen-random-table (max lap n)
  (declare (type (integer 1 100000000) max n)
	   (type (integer 1 1000000) lap))
  (let ((intervals (make-hash-table :test 'equal :size n))
	(result (make-interval-table #'< :bounds :closed-open)))
    (loop while (< (hash-table-count intervals) n)
	  do
	  (let* ((lo (random max))
		 (size (random lap))
		 (hi (+ lo size 1)))
	    (setf (gethash (cons lo hi) intervals) lo)
	    (setf (get-interval lo hi result) lo)))
    (values result intervals)))

(defun ht-containing (table point)
  (declare (type hash-table table))
  (let ((result 0))
    (declare (type integer result))
    (maphash #'(lambda (key val)
		 (let ((lo (car key)) (hi (cdr key)))
		   (when (and (>= point lo) (< point hi))
		     (incf result val))))
	     table)
    result))

(defun third-arg (a b c)
  (declare (ignore a b))
  c)

(defun make-bgroup (size)
  (multiple-value-bind (table hash-table)
      (gen-random-table size 20 size)
    (declare (type interval-table table)
	     (type hash-table hash-table))
    (let ((point (truncate (* 1/3 size))))
      (declare (type (integer 0 100000000) point))
      (list size
       (list "create"
	     (bench "interval-table"
		    #'(lambda (table)
			(maphash #'(lambda (key val)
				     (setf (get-interval (car key) (cdr key) table) val))
				 hash-table))
		    :init (make-interval-table #'< :bounds :closed-open)
		    :per-run t)
	     (bench "hash-table"
		    #'(lambda (table)
			(maphash #'(lambda (key val)
				     (setf (gethash key table) val))
				 hash-table))
		    :init (make-hash-table :test 'equal)
		    :per-run t))
       (list "properties"
	     (bench "empty-p" (interval-table-empty-p table))
	     (bench "count" (interval-table-count table))
	     (bench "ht-count" (hash-table-count hash-table))
	     (bench "every" (every-interval (constantly t) table))
	     (bench "some" (some-interval (constantly nil) table)))
       (list "lookup"
	     (bench "get-min" (get-min table))
	     (bench "get-max" (get-max table))
	     (bench "gethash" (gethash (cons point (+ point 22)) hash-table))
	     (bench "get-interval" (get-interval point (+ point 22) table)))
       (list "filter"
	     (bench "ht-containing" (ht-containing hash-table point))
	     (bench "containing" (map-intervals nil #'third-arg table :containing point))
	     (bench "intersecting" (map-intervals nil #'third-arg table :intersecting (list point (+ point 10))))
	     (bench "above/below" (map-intervals nil #'third-arg table :above (- point 1) :below (+ point 11))))))))

(defun run-all-benchmarks ()
  (run-benchmarks (mapcar #'make-bgroup '(100000 1000000))
		  :time 5 :report :tree))
