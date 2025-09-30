(defpackage :interval-tables
  (:use :cl)
  (:import-from :alexandria :array-length)
  (:export :interval-table :make-interval-table :interval-table-p
	   :interval-table-count :interval-table-empty-p
	   :get-interval :rem-interval
	   :do-intervals :map-intervals :subtable :map-values))

(in-package :interval-tables)

(deftype interval-bounds () '(member :open :closed :closed-open :open-closed))

(declaim (inline make-node node-lo node-hi node-max-upper
		 node-value node-left node-right))

(defstruct node
  lo hi ; TODO: should these be read-only?
  max-upper
  (left nil :type (or null node))
  (right nil :type (or null node))
  value)

#+SBCL (declaim (sb-ext:freeze-type node))

(defmacro %lt (a b) `(funcall %less ,a ,b))
(defmacro %gt (a b) `(funcall %less ,b ,a))
(defmacro %le (a b) `(not (funcall %less ,b ,a)))
(defmacro %ge (a b) `(not (funcall %less ,a ,b)))

(defmacro %max (a b)
  (let ((x (gensym))
	(y (gensym)))
    `(let ((,x ,a)
	   (,y ,b))
       (if (%lt ,x ,y) ,y ,x))))

(defun reset-max-upper (%less node)
  (declare (type function %less)
	   (type node node))
  (setf (node-max-upper node)
	(if (node-left node)
	    (if (node-right node)
		(%max (node-hi node) (%max (node-max-upper (node-left node)) (node-max-upper (node-right node))))
		(%max (node-hi node) (node-max-upper (node-left node))))
	    (if (node-right node)
		(%max (node-hi node) (node-max-upper (node-right node)))
		(node-hi node)))))

(declaim (inline compare compare-entries compare-interval-to-node))

(defun compare (%less a b)
  (cond ((%lt a b) -1)
	((%gt a b)  1)
	(t          0)))

(defun compare-nodes (less a b)
  (let ((r (compare less (node-lo a) (node-lo b))))
    (if (/= r 0)
	r
	(compare less (node-hi a) (node-hi b)))))

(defun compare-interval-to-node (less lo hi node)
  (let ((r (compare less lo (node-lo node))))
    (if (/= r 0)
	r
	(compare less hi (node-hi node)))))

(defun insert (%less tree lo hi value)
  (declare
   (type function %less)
   (type (or null node) tree)
   (optimize speed))
  (labels ((ins (e)
	     (if (null e)
		 (make-node :lo lo :hi hi :value value
			       :max-upper hi)
		 (let ((r (compare-interval-to-node %less lo hi e)))
		   (cond ((< r 0) (setf (node-left e) (ins (node-left e))))
			 ((> r 0) (setf (node-right e) (ins (node-right e))))
			 (t (setf (node-value e) value)))
                  (setf (node-max-upper e) (%max (node-max-upper e) hi))
                  e))))
    (ins tree)))


(declaim (inline %make-interval-table interval-table-less interval-table-bounds
		 interval-table-tree))

(defstruct (interval-table (:constructor %make-interval-table))
  (less nil :type (function (t t) boolean) :read-only t)
  (bounds :closed :type interval-bounds :read-only t)
  (tree nil :type (or null node)))

#+SBCL (declaim (sb-ext:freeze-type interval-table))

(declaim (ftype (function (interval-table) array-length) interval-table-count))

(defun interval-table-count (table)
  "The number of entries in the table. O(n)."
  (declare (optimize speed))
  (labels ((sum (n node)
	     (declare (type array-length n))
	     (if (null node)
		 n
		 (sum (sum (+ n 1) (node-left node)) (node-right node)))))
    (sum 0 (interval-table-tree table))))

(declaim (ftype (function (interval-table) array-length) depth))
(defun depth (table)
  (declare (optimize speed))
  (labels ((walk (node)
	     (if (null node)
		 0
		 (+ 1 (max (walk (node-left node))
			   (walk (node-right node)))))))
    (declare (ftype (function ((or null node)) array-length) walk))
    (walk (interval-table-tree table))))

	     
(declaim (ftype (function (interval-table) boolean) interval-table-empty-p))

(defun interval-table-empty-p (table)
  "Is the table empty? O(1)."
  (null (interval-table-tree table)))


(declaim
 (ftype (function ((or symbol (function (t t) boolean))
		   &key
		   (:bounds interval-bounds)
		   (:initial-contents sequence)
		   (:initial-contents* sequence))
		  interval-table)
	make-interval-table))

(defun make-interval-table (less &key
				   (bounds :closed)
				   initial-contents
				   initial-contents*)
  "Create a new interval table.
LESS must be a binary predicate that defines a strict total ordering on the set of interval bounds.
BOUNDS must be one of :CLOSED, :OPEN, :CLOSED-OPEN, :OPEN-CLOSED. It defaults to :CLOSED.
INITIAL-CONTENTS is a list of (lo hi value) lists that become the initial elements of the table.
INITIAL-CONTENTS* is a list of (lo hi . value) dotted lists that become the initial elements of the table."
  (let ((less (if (symbolp less) (symbol-function less) less)))
    (%make-interval-table
     :less less
     :bounds bounds
     :tree (reduce #'(lambda (tree e)
		       (destructuring-bind (lo hi value) e
			 (insert less tree lo hi value)))
		   initial-contents
		   :initial-value
		   (reduce #'(lambda (tree e)
			       (destructuring-bind (lo hi &rest value) e
				 (insert less tree lo hi value)))
			   initial-contents*
			   :initial-value nil)))))


(declaim (ftype (function (t t interval-table &optional t) (values t boolean)) get-interval))

(defun get-interval (lo hi table &optional default)
  "Look up the interval with bounds lo,hi in table.
If the table contains the interval, returns its value and t.
If not, returns default and nil."
  (let ((%less (interval-table-less table)))
    (labels ((walk (node)
	       (if (null node)
		   (values default nil)
		   (let ((r (compare-interval-to-node %less lo hi node)))
		     (cond ((< r 0) (walk (node-left node)))
			   ((> r 0) (walk (node-right node)))
			   (t (values (node-value node) t)))))))
      (walk (interval-table-tree table)))))

(declaim (ftype (function (t t interval-table) boolean) empty-interval-p))
(defun empty-interval-p (lo hi table)
  (let ((%less (interval-table-less table)))
    (if (eq (interval-table-bounds table) :closed)
	(%lt hi lo)
	(%le hi lo))))

(declaim (ftype (function (t t interval-table t) t) interval-table-put))
(defun interval-table-put (lo hi table value)
  "Associate the interval lo,hi with value in the table. Returns value."
  (when (empty-interval-p lo hi table)
    (error "empty interval ~S ~S ~S" lo hi (interval-table-bounds table)))
  (setf (interval-table-tree table)
	(insert (interval-table-less table)
		(interval-table-tree table)
		lo hi value))
  value)

(defsetf get-interval (lo hi table &optional default) (new-value)
  (declare (ignore default))
  `(interval-table-put ,lo ,hi ,table ,new-value))

(declaim (ftype (function (t t interval-table) boolean) rem-interval))
(defun rem-interval (lo hi table)
  "Remove the entry for interval lo,hi in table, if any.
Returns true if there was such an entry, or false otherwise."
  TODO)

(defun before-node-p (tab node point)
  (let ((%less (interval-table-less tab)))
    (ecase (interval-table-bounds tab)
      ((:closed :closed-open) (%lt point (node-lo node)))
      ((:open :open-closed)   (%le point (node-lo node))))))

(defun above-upper-bound-p (tab upper point)
  (let ((%less (interval-table-less tab)))
    (ecase (interval-table-bounds tab)
      ((:closed :open-closed) (%gt point upper))
      ((:open :closed-open) (%ge point upper)))))

(defun node-contains-point-p (tab node point)
  (let ((%less (interval-table-less tab))
        (lo (node-lo node))
        (hi (node-hi node)))
    (ecase (interval-table-bounds tab)
      (:closed (and (%le lo point) (%le point hi)))
      (:open (and (%lt lo point) (%lt point hi)))
      (:closed-open (and (%le lo point) (%lt point hi)))
      (:open-closed (and (%lt lo point) (%le point hi))))))


(declaim (ftype (function (interval-table function) null) map-values))
(defun map-values (table function)
  "Update all values in table by the result of calling function with the lower bound, upper bound, and value."
  (labels ((walk (node)
	     (declare (type (or null node) node))
	     (when node
	       (setf (node-value node) (funcall function (node-lo node) (node-hi node) (node-value node)))
	       (walk (node-left node))
	       (walk (node-right node)))))
    (walk (interval-table-tree table))))


(defun subtable (table &key containing not-containing intersecting not-intersecting above below)
  (declare (type interval-table table))
  TODO)

(defun %map-to-list (function table from-end)
  (declare (type function function)
	   (type interval-table table))
  (labels ((collect (e xs)
	     (declare (type (or null node) e)
		      (type list xs))
             (if (null e)
                 xs
                 (collect
		  (node-left e)
                  (cons (funcall function (node-lo e) (node-hi e) (node-value e))
                        (collect (node-right e) xs)))))
	   (collect-from-end (e xs)
	     (declare (type (or null node) e)
		      (type list xs))
             (if (null e)
                 xs
                 (collect-from-end
		  (node-right e)
                  (cons (funcall function (node-lo e) (node-hi e) (node-value e))
                        (collect-from-end (node-left e) xs))))))
    (if from-end
	(collect-from-end (interval-table-tree table) nil)
	(collect (interval-table-tree table) nil))))

(defun %for-each (function table from-end)
  (declare (type function function)
	   (type interval-table table))
  (labels ((walk (e)
	     (declare (type (or null node) e))
             (when e
               (walk (node-left e))
               (funcall function (node-lo e) (node-hi e) (node-value e))
               (walk (node-right e))))
	   (walk-from-end (e)
	     (declare (type (or null node) e))
             (when e
               (walk-from-end (node-right e))
               (funcall function (node-lo e) (node-hi e) (node-value e))
               (walk-from-end (node-left e)))))
    (if from-end
	(walk-from-end (interval-table-tree table))
	(walk (interval-table-tree table)))))

(defmacro do-intervals (((lo hi) value table) &body body)
  "Iterate over the entries in table."
  (let ((proc (gensym))
        (e (gensym))
        (tab (gensym)))
    `(let ((,tab ,table))
       (labels ((,proc (,e)
		  (declare (type (or null node) ,e))
                  (when ,e
                    (,proc (node-left ,e))
                    (let ((,lo (node-lo ,e))
                          (,hi (node-hi ,e))
                          (,value (node-value ,e)))
                      (declare (ignorable ,lo ,hi ,value))
                      ,@body)
                    (,proc (node-right ,e)))))
       (,proc (interval-table-tree ,tab))))))

(declaim (ftype (function (t function interval-table
			     &key
			     (:above t)
			     (:below t)
			     (:containing t)
			     (:from-end t)
			     (:intersecting (or null (cons * (cons * null)))))
			  (or list vector))
		map-intervals))

(defun map-intervals (result-type function table &key containing intersecting above below from-end)
  (cond ((null result-type) (%for-each function table from-end))
        ((eq result-type 'list) (%map-to-list function table from-end))
        ;; TODO: string vector, length check
	;; string (string 4711) (string *) vector (vector type [size]) (array type (size))
        (t (error "invalid result-type ~S" result-type))))


(defun containing-point (table point)
  "Return list of entries where point is within the lo-hi interval."
  (labels ((walk (xs node)
	     (cond ((null node) xs)
		   ((above-upper-bound-p table (node-max-upper node) point)
		    ;; point is above the maximum upper bound: no result
		    xs)
		   ((before-node-p table node point)
		     ;; point is to the left of the node: can't be in right subtree
		    (walk xs (node-left node)))
		   ((node-contains-point-p table node point)
		    (walk (cons node (walk xs (node-right node)))
			  (node-left node)))
		   (t
		    (walk (walk xs (node-right node)) (node-left node))))))
    (walk nil (interval-table-tree table))))

