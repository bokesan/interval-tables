(in-package :cl-user)

(defpackage :tests
  (:use :cl :check-it :interval-tables)
  (:import-from :alexandria :array-length)
  (:import-from :fiveam :test :is :is-true :is-false))

(in-package :tests)

(fiveam:def-suite tests)
(fiveam:in-suite tests)

;; (setf fiveam:*on-error* :debug)

(setf check-it:*num-trials* 1000)


(def-generator gen-interval-table ()
  (generator
   (map #'(lambda (bounds items)
	    (make-interval-table
	     #'<
	     :bounds (aref #(:closed :open :closed-open :open-closed) bounds)
	     :initial-contents*
	     (mapcar #'(lambda (xs)
			 (destructuring-bind (lo size val)
			     xs
			   (let ((size (if (= bounds 0) size (+ size 1))))
			     (list* lo (+ lo size) val))))
		     items)))
	(integer 0 3)
	(list (tuple (integer) (integer 0 *) (string))))))

(defun third-arg (a b c)
  (declare (ignore a b))
  c)

(defvar *table* )

(test basics
      (let ((table (make-interval-table
		    '< ; symbol instead of function
		    :initial-contents
		    '((1 2 a) (6 9 b) (4 7 c) (2 6 d)))))
	(is (= 4 (interval-table-count table)))
	(is (eq 'a (get-interval 1 2 table)))
	(is (eq 'c (get-interval 4 7 table)))
	(is (null (get-interval 4 6 table)))
	(is (string= "foo" (get-interval 4 6 table "foo")))
	(is-false (delete-min (make-interval-table #'<)))
	))

(test readme
      ;; The examples in the README should better work
      (let ((table (make-interval-table #'<)))
	(is (= 0 (interval-table-count table)))
	(is-true (interval-table-empty-p table))
	(setf (get-interval 1 10 table) "E1")
	(setf (get-interval 11 20 table) "E2")
	(setf (get-interval 5 15 table) "E3")
	(setf (get-interval 0 2 table) "E4")
	(setf (get-interval 5 15 table) "E5")
	(is (= 4 (interval-table-count table)))
	(is-false (interval-table-empty-p table))
	(multiple-value-bind (value present-p) (get-interval 5 15 table)
	  (is (string= "E5" value))
	  (is-true present-p))
	(multiple-value-bind (value present-p) (get-interval 1 4 table)
	  (is (null value))
	  (is-false present-p))
	(multiple-value-bind (value present-p) (get-interval 1 4 table "Nope")
	  (is (string= "Nope" value))
	  (is-false present-p))
	(is (equal '("E4" "E1" "E5" "E2") (map-intervals 'list #'third-arg table)))
	(is (equal '("E2" "E5" "E1" "E4") (map-intervals 'list #'third-arg table :from-end t)))
	(multiple-value-bind (lo hi value)
	    (delete-min table)
	  (is (= 0 lo))
	  (is (= 2 hi))
	  (is (string= "E4" value))
	  (is (equal '("E1" "E5" "E2") (map-intervals 'list #'third-arg table))))
	))


(test prop-set-get
      (is-true
       (check-it
	(generator (tuple (gen-interval-table) (integer) (integer 1 *) (string)))
	(lambda (x)
	  (destructuring-bind (table lo size value) x
	    (declare (type interval-table table)
		     (type integer lo size)
		     (type string value))
	    (let* ((old-count (interval-table-count table))
		   (hi (+ lo size))
		   (old-value (get-interval lo hi table)))
	      (setf (get-interval lo hi table) value)
	      (and (string= (get-interval lo hi table) value)
		   (= (interval-table-count table)
		      (if old-value old-count (+ old-count 1))))))))))

(defun cmp-intervals (lo1 hi1 lo2 hi2)
  (declaim (type integer lo1 hi1 lo2 hi2))
  (cond ((< lo1 lo2) -1)
	((> lo1 lo2)  1)
	((< hi1 hi2) -1)
	((> hi1 hi2)  1)
	(t 0)))

(test prop-get-min
      (is-true
       (check-it
	(generator (gen-interval-table))
	(lambda (table)
	  (multiple-value-bind (lo hi value)
	      (get-min table)
	    (or (and (interval-table-empty-p table)
		     (null lo) (null hi) (null value))
		(and (not (interval-table-empty-p table))
		     (every-interval #'(lambda (a b c)
					 (declare (ignore c))
					 (<= (cmp-intervals lo hi a b) 0))
				     table))))))))

(test prop-get-max
      (is-true
       (check-it
	(generator (gen-interval-table))
	(lambda (table)
	  (multiple-value-bind (lo hi value)
	      (get-max table)
	    (or (and (interval-table-empty-p table)
		     (null lo) (null hi) (null value))
		(and (not (interval-table-empty-p table))
		     (every-interval #'(lambda (a b c)
					 (declare (ignore c))
					 (>= (cmp-intervals lo hi a b) 0))
				     table))))))))

(test prop-delete-min
      (is-true
       (check-it
	(generator (gen-interval-table))
	(lambda (table)
	  (declare (type interval-table table))
	  (let ((old-count (interval-table-count table)))
	    (multiple-value-bind (lo hi value)
		(delete-min table)
	      (if (zerop old-count)
		  (not (or lo hi value))
		  (and (= (interval-table-count table) (- old-count 1))
		       (every-interval #'(lambda (a b c)
					   (declare (ignore c))
					   (interval< lo hi a b))
				       table)))))))))

	    
(defun max-depth (n)
  (declare (type array-length n))
  (if (= n 0)
      0
      (1+ (* 1.4 (log n 2)))))

(test prop-depth
      (is-true
       (check-it
	(generator (gen-interval-table))
	(lambda (table)
	  (declare (type interval-table table))
	  (<= (interval-tables::height table)
	      (max-depth (interval-table-count table)))))))
