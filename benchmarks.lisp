(defpackage :benchmarks
  (:use :cl :interval-tables :low-crit)
  (:export :run-all-benchmarks))

(in-package :benchmarks)

(defun gen-random-table (max lap n)
  (declare (type (integer 1 100000000) max n)
	   (type (integer 1 1000000) lap))
  (let ((result (make-interval-table #'<)))
    (dotimes (i n result)
      (let ((lo (1+ (random max)))
	    (size (random lap)))
	(setf (get-interval lo (+ lo size) result) lo)))))

(defparameter *data-size* 10000)

(defun third-arg (a b c)
  (declare (ignore a b))
  c)

(defvar *benchmarks*
  (let ((table (gen-random-table *data-size* 20 *data-size*)))
    (bgroup "all"
      (bench "create" (gen-random-table *data-size* 20 *data-size*))
      (bench "count" (interval-table-count table))
      (bench "empty-p" (interval-table-empty-p table))
      (bench "get-min" (get-min table))
      (bench "get-max" (get-max table))
      (bench "get-interval" (get-interval 10000 25 table))
      (bench "containing" (map-intervals nil #'third-arg table :containing 10017))
      (bench "intersecting" (map-intervals nil #'third-arg table :intersecting '(10000 10009)))
      (bench "above/below" (map-intervals nil #'third-arg table :above 9999 :below 100010))
      (bench "every" (every-interval #'(lambda (a b c)
					 (declare (ignore a b))
					 (numberp c))
				     table))
      (bench "some" (some-interval #'(lambda (a b c)
				       (declare (ignore a b))
				       (stringp c))
				   table)))))

(defun run-all-benchmarks ()
  (let ((*seconds-per-benchmark* 10)
	(*verbose* nil))
    (run-benchmarks *benchmarks*)))
