(in-package :cl-user)

(defpackage :tests
  (:use :cl :check-it :interval-tables)
  (:import-from :fiveam :test :is))

(in-package :tests)

(fiveam:def-suite tests)
(fiveam:in-suite tests)

(setf fiveam:*on-error* :debug)

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

(defvar *table* (make-interval-table #'<
                                     :initial-contents
				     '((1 2 a) (6 9 b)
                                       (4 7 c) (2 6 d))))
(test basics
      (is (= 4 (interval-table-count *table*)))
      (is (eq 'a (interval-table-ref *table* 1 2)))
      (is (eq 'c (interval-table-ref *table* 4 7)))
      (is (null (interval-table-ref *table* 4 6))))

(test prop-set-get
      (let ((*num-trials* 1000))
	(is (check-it
	     (generator (tuple (gen-interval-table) (integer) (integer 1 *) (string)))
	     (lambda (x)
	       (destructuring-bind (table lo size value) x
		 (let* ((old-count (interval-table-count table))
			(hi (+ lo size))
			(old-value (get-interval table lo hi)))
		   (setf (get-interval table lo hi) value)
		   (and (string= (get-interval table lo hi) value)
			(= (interval-table-count table)
			   (if old-value old-count (+ old-count 1)))))))))))
