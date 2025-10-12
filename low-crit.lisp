(defpackage :low-crit
  (:use :cl)
  (:export :run-benchmarks :bench :bgroup))

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

(declaim (inline make-benchmark benchmark-p benchmark-name benchmark-proc
		 benchmake-init benchmark-cleanup benchmark-per-run-p))
(defstruct benchmark
  (name nil :type string :read-only t)
  (proc nil :type function :read-only t)
  (init nil :type (or null function) :read-only t)
  (cleanup nil :type (or null function) :read-only t)
  (per-run-p nil :read-only t))

#+sbcl (declaim (sb-ext:freeze-type benchmark))


(declaim
 (ftype (function ((fast-integer 1) function) integer) run-repeatedly)
 (ftype (function ((fast-integer 1) function t) integer) run-repeatedly-with-env)
 (ftype (function ((fast-integer 1) benchmark) integer) run-repeatedly-with-env-per-run))

(defun run-repeatedly (iters proc)
  (declare (type (fast-integer 1) iters)
	   (type function proc))
  (let ((start (get-internal-run-time)))
    (declare (optimize speed (safety 0)))
    (dotimes (i iters)
      (funcall proc))
    (- (get-internal-run-time) start)))

(defun run-repeatedly-with-env (iters proc env)
  (declare (type (fast-integer 1) iters)
	   (type function proc))
  (let ((start (get-internal-run-time)))
    (declare (optimize speed (safety 0)))
    (dotimes (i iters)
      (funcall proc env))
    (- (get-internal-run-time) start)))

(defun run-repeatedly-with-env-per-run (iters bm)
  (declare (type (fast-integer 1) iters)
	   (type benchmark bm))
  (let ((time 0))
    (dotimes (i iters)
      (let* ((env (funcall (benchmark-init bm)))
	     (start (get-internal-run-time)))
	(funcall (benchmark-proc bm) env)
	(incf time (max 0 (- (get-internal-run-time) start)))
	(when (benchmark-cleanup bm)
	  (funcall (benchmark-cleanup bm) env))))
    time))

(declaim (notinline run-repeatedly run-repeatedly-with-env run-repeatedly-with-env-per-run))



(declaim (ftype (function ((or benchmark function) &key (:time real))
			  (values real unsigned-byte real))
		measure))
(defun measure (benchmark &key (time *seconds-per-benchmark*))
  (let ((timeout (* time internal-time-units-per-second))
	(env (if (and (benchmark-p benchmark) (benchmark-init benchmark))
		 (funcall (benchmark-init benchmark))
		 nil))
	(per-run-p (and (benchmark-p benchmark) (benchmark-per-run-p benchmark)))
	(proc (if (benchmark-p benchmark) (benchmark-proc benchmark) benchmark))
	(start (get-internal-real-time)))
    (declare (type function proc))
    (flet ((run (iters)
	     (cond ((null env) (run-repeatedly iters proc))
		   ((not per-run-p) (run-repeatedly-with-env iters proc env))
		   (t (run-repeatedly-with-env-per-run iters benchmark)))))
      (do* ((iters 1 (ceiling (* iters +growth-factor+)))
	    (result (run iters) (run iters))
	    (elapsed (- (get-internal-real-time) start)
		     (- (get-internal-real-time) start)))
	   ((> elapsed timeout)
	    (values (/ result (* iters internal-time-units-per-second))
		    iters
		    (/ elapsed internal-time-units-per-second)))))))

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
	(format t "Time: ~A: ~A overhead~%"
		(format-seconds time)
		(format-seconds (measure *noop* :time time)))))

(defun label-p (obj)
  (or (stringp obj)
      (characterp obj)
      (symbolp obj)
      (numberp obj)))

(defun run-benchmarks (benchmarks &key (time *seconds-per-benchmark*) (progress :flat) verbose)
  (declare (type (member :none :flat :tree) progress)
	   (type real time))
  (let ((*verbose* verbose)
	(indent 4))
    (when *verbose*
      (format t "~&Running benchmarks...~%"))
    (labels ((run-bm (path name proc)
	       (let ((overhead (measure *noop* :time 0.4)))
		 (multiple-value-bind (mean iters elapsed)
		     (measure proc :time time)
		   (case progress
		     (:flat (format t "~&~A:~40,2T~A"
				    (stringify-path path name)
				    (format-seconds (- mean overhead))))
		     (:tree (format t "~&~v<~>~A:~30,2T~A"
				    (* indent (length path))
				    name
				    (format-seconds (- mean overhead)))))
		   (when (and *verbose* (not (eq progress :none)))
		     (format t "  (iters: ~11D, overhead: ~A, elapsed: ~A)"
			     iters
			     (format-seconds overhead)
			     (format-seconds elapsed)))
		   (unless (eq progress :none)
		     (terpri)))))
	     (run (path bm)
	       (declare (type list path))
	       (cond ((benchmark-p bm) (run-bm path (benchmark-name bm) bm))
		     ((functionp bm) (run-bm path (format nil "~S" bm) bm))
		     ((consp bm)
		      (cond ((label-p (car bm))
			     (when (eq progress :tree)
			       (format t "~v<~>~A~%" (* indent (length path)) (car bm)))
			     (setq path (cons (car bm) path))
			     (dolist (b (cdr bm))
			       (run path b)))
			    (t
			     (dolist (b bm)
			       (run path b)))))
		     (t (error "invalid benchmark: ~S" bm)))))
      (run nil benchmarks))))

(defmacro bench (name form &key init cleanup per-run)
  "Define a single benchmark test."
  `(make-benchmark :name ,name
		   :proc ,(if (and (consp form) (eq (car form) 'function))
			      form
			      `(function (lambda () ,form)))
		   ,@(if init `(:init #'(lambda () ,init)) nil)
		   ,@(if cleanup `(:cleanup #'(lambda () ,cleanup)) nil)
		   ,@(if per-run '(:per-run-p t) nil)))
