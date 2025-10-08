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
	(is (equal '((1 10 "E1") (5 15 "E5")) (map-intervals 'list #'list table :containing 9)))
	(is (equal '((5 15 "E5") (1 10 "E1")) (map-intervals 'list #'list table :from-end t :containing 9)))
	(is (equalp #("E5" "E2") (map-intervals 'vector #'third-arg table :intersecting '(13 50))))
	(is (equal '("E4" "E1" "E5" "E2") (map-intervals 'list #'third-arg table)))
	(is (equal '("E2" "E5" "E1" "E4") (map-intervals 'list #'third-arg table :from-end t)))
	(is (equal '(0 2 "E4") (multiple-value-list (get-min table))))
	(is (equal '(0 2 "E4") (multiple-value-list (delete-min table))))
	(is (equal '(1 10 "E1") (multiple-value-list (get-min table))))))

(test reverse-order
      ;; check that it works with "greater" instead of "less"
      (let ((table (make-interval-table
		    #'>
		    :initial-contents '((1 2 3) (2 4 6) (3 7 10) (4 5 9) (9 12 21) (6 6 12) (1 6 7)))))
	(is (= 12 (get-interval 6 6 table)))
	(is (= 10 (get-interval 3 7 table)))
	(is (equal '(9 12 21) (multiple-value-list (get-min table))))
	(is (equal '(1 2 3) (multiple-value-list (get-max table))))))

(test map-values
      (let ((table (make-interval-table #'< :initial-contents '((4 6 100) (2 3 10)))))
	(map-values #'(lambda (lo hi val) (format nil "~A-~A: ~A" lo hi val)) table)
	(is (string= "4-6: 100" (get-interval 4 6 table)))
	(is (string= "2-3: 10" (get-interval 2 3 table)))))

(test map-to-string
      (let ((table (make-interval-table #'< :initial-contents '((4 6 #\A) (2 3 #\C)))))
	(is (string= "CA" (map-intervals 'string #'third-arg table)))
	(is (string= "AC" (map-intervals 'string #'third-arg table :from-end t)))))

(test every-some
      (let ((table (make-interval-table
		    #'<
		    :initial-contents '((1 2 3) (2 4 6) (3 7 10) (4 5 9) (9 12 21) (6 6 12) (1 6 7)))))
	(is-true (every-interval #'(lambda (lo hi v) (= v (+ lo hi))) table))
	(is (equal '(2 4 6)  (some-interval #'(lambda (lo hi v) (if (evenp v) (list lo hi v) nil)) table)))
	(is (equal '(6 6 12) (some-interval #'(lambda (lo hi v) (if (evenp v) (list lo hi v) nil)) table :from-end t)))))

(test prop-set-get
      (is-true
       (check-it
	(generator (tuple (gen-interval-table) (integer) (integer 0 *) (string)))
	(lambda (x)
	  (destructuring-bind (table lo size value) x
	    (declare (type interval-table table)
		     (type integer lo size)
		     (type string value))
	    (let* ((old-count (interval-table-count table))
		   (hi (if (eq (interval-table-bounds table) :closed)
			   (+ lo size)
			   (+ lo size 1)))
		   (old-value (get-interval lo hi table)))
	      (setf (get-interval lo hi table) value)
	      (and (string= (get-interval lo hi table) value)
		   (= (interval-table-count table)
		      (if old-value old-count (+ old-count 1))))))))))

(defun cmp-intervals (lo1 hi1 lo2 hi2)
  (declare (type integer lo1 hi1 lo2 hi2))
  (cond ((< lo1 lo2) -1)
	((> lo1 lo2)  1)
	((< hi1 hi2) -1)
	((> hi1 hi2)  1)
	(t 0)))

(defun contains-p (bounds lo hi point)
  (ecase bounds
    (:closed      (<= lo point hi))
    (:open        (<  lo point hi))
    (:closed-open (and (<= lo point) (<  point hi)))
    (:open-closed (and (<  lo point) (<= point hi)))))

(defun intervals-intersect-p (closed-p lo1 hi1 lo2 hi2)
  (if closed-p
      (and (<= lo1 hi2) (>= hi1 lo2))
      (and (< lo1 hi2) (> hi1 lo2))))

(test prop-containing
      (is-true
       (check-it
	(generator (tuple (gen-interval-table) (integer)))
	(lambda (x)
	  (destructuring-bind (table point) x
	    (declare (type interval-table table)
		     (type integer point))
	    (let* ((bounds (interval-table-bounds table))
		   (expected (remove-if-not
			      #'(lambda (e)
				  (destructuring-bind (lo hi val) e
				    (contains-p bounds lo hi point)))
			      (map-intervals 'list #'list table)))
		   (result (map-intervals 'list #'list table :containing point)))
	      (equal expected result)))))))

(test prop-intersecting
      (is-true
       (check-it
	(generator (tuple (gen-interval-table) (integer) (integer 0 *)))
	(lambda (x)
	  (destructuring-bind (table lo size) x
	    (declare (type interval-table table)
		     (type integer lo size))
	    (let* ((closed-p (eq (interval-table-bounds table) :closed))
		   (hi (if closed-p (+ lo size) (+ lo size 1)))
		   (expected (remove-if-not
			      #'(lambda (e)
				  (destructuring-bind (a b val) e
				    (intervals-intersect-p closed-p lo hi a b)))
			      (map-intervals 'list #'list table)))
		   (result (map-intervals 'list #'list table :intersecting (list lo hi))))
	      (equal expected result)))))))
	  
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
					   (< (cmp-intervals lo hi a b) 0))
				       table)))))))))

(test prop-delete-interval
      (is-true
       (check-it
	(generator (tuple (gen-interval-table) (integer) (integer 0 *)))
	(lambda (x)
	  (destructuring-bind (table lo size) x
	    (declare (type interval-table table)
		     (type integer lo size))
	    (let* ((old-count (interval-table-count table))
		   (hi (if (eq (interval-table-bounds table) :closed)
			   (+ lo size)
			   (+ lo size 1)))
		   (old-value (get-interval lo hi table)))
	      (multiple-value-bind (deleted-value present-p)
		  (delete-interval lo hi table)
		(if (not old-value)
		    (and (null deleted-value)
			 (not present-p)
			 (= (interval-table-count table) old-count))
		    (and (string= deleted-value old-value)
			 present-p
			 (= (interval-table-count table) (- old-count 1)))))))))))

;;;; internal stuff

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

(test at-most-one-of
      (is-true (interval-tables::at-most-one-of nil nil 3 nil))
      (is-false (interval-tables::at-most-one-of nil nil 3 nil 1))
      (is-true (interval-tables::at-most-one-of))
      (is-false (interval-tables::at-most-one-of "a" "b")))
