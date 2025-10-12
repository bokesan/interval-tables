(defpackage :low-crit
  (:use :cl)
  (:export :run-benchmarks :bench :bgroup :*verbose* :*seconds-per-benchmark*))

(in-package :low-crit)

(declaim (type (real (0)) *seconds-per-benchmark*))
(defparameter *seconds-per-benchmark* 5)

(defvar *verbose* nil)

(defconstant +growth-factor+ 117/100)


(deftype fast-integer (&optional (min most-negative-fixnum) (max most-positive-fixnum))
  "Type designator for a fixnum with the given bounds."
  (unless (and (integerp min) (>= min most-negative-fixnum))
    (error "invalid lower bound for fast-integer ~S" min))
  (unless (and (integerp max) (<= max most-positive-fixnum))
    (error "invalid upper bound for fast-integer ~S" max))
  `(integer ,min ,max))

(declaim (inline with))
(defun with (unit time)
  (format nil
	  (cond ((>= time 1000000000) "~,4G ~A")
		((>= time       1000) "~,0F ~A")
		((>= time        100) "~,1F ~A")
		((>= time         10) "~,2F ~A")
		(t                    "~,3F ~A"))
	  time unit))

(declaim (ftype (function (real) string) format-internal-time))
(defun format-seconds (k)
  "Convert seconds to a string.
The string will consist of four decimal places,
followed by a short description of the time units."
  (if (< k 0)
      (format nil "-~A" (format-seconds (- k)))
      (let ((e 0))
	(dolist (u '("s " "ms" "µs" "ns" "ps" "fs" "as" "zs" "ys" "rs" "qs"))
	  (when (>= k (expt 10 (- e)))
	    (return-from format-seconds (with u (* k (expt 10 e)))))
	  (incf e 3))
	(format nil "~G s" k))))


(declaim (ftype (function ((fast-integer 1) function) real) run-repeatedly))
(defun run-repeatedly (iters proc)
  (declare (type (fast-integer 1) iters)
	   (type function proc))
  (let ((start (get-internal-run-time)))
    (declare (optimize speed (safety 0)))
    (dotimes (i iters)
      (funcall proc))
    (/ (- (get-internal-run-time) start) internal-time-units-per-second)))

(declaim (notinline run-repeatedly))


(declaim (ftype (function (function) (values real unsigned-byte real)) measure))
(defun measure (proc)
  (let ((timeout (* *seconds-per-benchmark* internal-time-units-per-second))
	(start (get-internal-real-time)))
    (do* ((iters 1 (ceiling (* iters +growth-factor+)))
	  (result (run-repeatedly iters proc)
		  (run-repeatedly iters proc))
	  (elapsed (- (get-internal-real-time) start)
		   (- (get-internal-real-time) start)))
	((> elapsed timeout)
	 (values (/ result iters) iters (/ elapsed internal-time-units-per-second)))
      (setq result (run-repeatedly iters proc)))))

(declaim (ftype (function (list string) string) stringify-path))
(defun stringify-path (path name)
  (if (null path)
      name
      (format nil "~{~A~^/~}" (reverse (cons name path)))))

(defvar *noop* #'(lambda () 0))

(defun test-overhead ()
  (loop for time = 5 then (* time 2/3)
	while (>= time 1/1000)
	do
	(let ((*seconds-per-benchmark* time))
	  (format t "Time: ~A: ~A overhead~%"
		  (format-seconds *seconds-per-benchmark*)
		  (format-seconds (measure *noop*))))))



(defun run-benchmarks (&rest benchmarks-and-groups)
  (let ((overhead (measure *noop*)))
    (when *verbose*
      (format t "~&nocrit overhead: ~A. Running benchmarks...~%" (format-seconds overhead)))
    (labels ((run (path bm)
	       (declare (type list path bm))
	       (ecase (car bm)
		 (bgroup
		  (let ((name (cadr bm))
			(benches (cddr bm)))
		    (map nil #'(lambda (b) (run (cons name path) b)) benches)))
		 (bench
		  (let ((name (cadr bm))
			(thunk (caddr bm))
			(overhead (let ((*seconds-per-benchmark* 0.5))
				    (measure *noop*))))
		    (multiple-value-bind (mean iters elapsed)
			(measure thunk)
		      (format t "~&~A:~40,2T~A"
			      (stringify-path path name)
			      (format-seconds (- mean overhead)))
		      (when *verbose*
			(format t "  (iters: ~11D, overhead: ~A, elapsed: ~A)"
				iters
				(format-seconds overhead)
				(format-seconds elapsed)))
		      (terpri)))))))
      (dolist (b benchmarks-and-groups)
	(run nil b)))))

(defun bgroup (name &rest benchmarks-and-groups)
  "Group benchmarks/groups under one name."
  `(bgroup ,(string name) ,@benchmarks-and-groups))

(defmacro bench (arg1 &rest args)
  "Define a single benchmark test.
Can take various forms:
(bench form ...)
(bench name form ...)"
  (let ((name "")
	(forms args))
    (declare (type string name))
    (if (or (stringp arg1) (symbolp arg1) (characterp arg1))
	(setq name (string arg1))
	(setq name (format nil "~S" arg1)
	      forms (cons arg1 args)))
    `(list 'bench ,name #'(lambda () ,@forms))))
