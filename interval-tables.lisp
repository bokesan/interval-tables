(defpackage :interval-tables
  (:use :cl)
  (:import-from :alexandria :array-length)
  (:export :interval-table :make-interval-table :interval-table-p
	   :interval-table-bounds
	   :interval-table-count :interval-table-empty-p
	   :get-interval
	   :get-min :get-max :get-last
	   :delete-interval
	   :delete-min :delete-max :delete-last
	   :do-intervals :map-intervals :subtable :map-values
	   :every-interval :some-interval))

(in-package :interval-tables)

(defun at-most-one-of (&rest values)
  "Check that at most one of the arguments is true."
  (labels ((look (xs one-p)
	     (declare (type list xs))
	     (cond ((null xs) t)
		   ((car xs) (if one-p
				 nil
				 (look (cdr xs) t)))
		   (t (look (cdr xs) one-p)))))
    (look values nil)))


(deftype interval-bounds () '(member :open :closed :closed-open :open-closed))

(declaim (inline make-node node-lo node-hi node-max-upper
		 node-value node-left node-right
		 node-red-p))

(defstruct node
  lo
  hi
  max-upper
  (left  nil :type (or null node))
  (right nil :type (or null node))
  (red-p t)
  value)

#+SBCL (declaim (sb-ext:freeze-type node))

(defmacro %lt (a b) `(funcall %less ,a ,b))
(defmacro %gt (a b) `(funcall %less ,b ,a))
(defmacro %le (a b) `(not (funcall %less ,b ,a)))
(defmacro %ge (a b) `(not (funcall %less ,a ,b)))

(defmacro %max (a b)
  (let ((x (gensym)) (y (gensym)))
    `(let ((,x ,a) (,y ,b))
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


(declaim (inline red-p flip-color))
  
(defun red-p (node)
  "Is node non-null and red?"
  (and node (node-red-p node)))

(defun flip-color (h)
  "Flip the color of a single node."
  (setf (node-red-p h) (not (node-red-p h))))

(defun color-flip (h)
  "Flip colors of a node and its direct children."
  (declare (type node h))
  (flip-color h)
  (flip-color (node-left h))
  (flip-color (node-right h)))

(declaim (ftype (function (function node) node) rotate-left))
(defun rotate-left (%less h)
  (declare (type node h))
  (let ((x (node-right h)))
    (declare (type node x))
    (setf (node-right h) (node-left x))
    (setf (node-left x) h)
    (setf (node-red-p x) (node-red-p h))
    (setf (node-red-p h) t)
    (reset-max-upper %less h)
    (reset-max-upper %less x)
    x))

(declaim (ftype (function (function node) node) rotate-right))
(defun rotate-right (%less h)
  (declare (type node h))
  (let ((x (node-left h)))
    (declare (type node x))
    (setf (node-left h) (node-right x))
    (setf (node-right x) h)
    (setf (node-red-p x) (node-red-p h))
    (setf (node-red-p h) t)
    (reset-max-upper %less h)
    (reset-max-upper %less x)
    x))


(declaim (ftype (function (function node) node) fix-up))
(defun fix-up (%less h)
  "Preserve RB-tree property after inserting a new node as left or right child of h."
  (declare (type node h))
  (reset-max-upper %less h)
  (when (and (red-p (node-right h)) (not (red-p (node-left h))))
    (setq h (rotate-left %less h)))
  (when (and (red-p (node-left h)) (red-p (node-left (node-left h))))
    (setq h (rotate-right %less h)))
  (when (and (red-p (node-left h)) (red-p (node-right h)))
    (color-flip h))
  h)

(defun move-red-left (%less h)
  (declare (type node h))
  (color-flip h)
  (when (red-p (node-left (node-right h)))
    (setf (node-right h) (rotate-right %less (node-right h)))
    (setq h (rotate-left %less h))
    (color-flip h))
  h)

(defun move-red-right (%less h)
  (declare (type node h))
  (color-flip h)
  (when (red-p (node-left (node-left h)))
    (setq h (rotate-right %less h))
    (color-flip h))
  h)

(declaim (ftype (function (function (or null node) t t t) node) insert))

(defun insert (%less tree lo hi value)
  (declare (type function %less)
	   (type (or null node) tree)
	   (optimize speed))
  (labels ((ins (e)
	     (declare (type (or null node) e))
	     (if (null e)
		 (make-node :lo lo :hi hi :max-upper hi :value value)
		 (let ((r (compare-interval-to-node %less lo hi e)))
		   (cond ((< r 0)
			  (setf (node-left e) (ins (node-left e)))
			  (fix-up %less e))
			 ((> r 0)
			  (setf (node-right e) (ins (node-right e)))
			  (fix-up %less e))
			 (t
			  (setf (node-value e) value)
			  e))))))
    (ins tree)))



(declaim (inline %make-interval-table interval-table-less interval-table-bounds
		 interval-table-tree))

(defstruct (interval-table (:constructor %make-interval-table)
			   (:print-object print-ivt))
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

(declaim (ftype (function (interval-table) array-length) height))
(defun height (table)
  (declare (optimize speed))
  (labels ((walk (node)
	     (if (null node)
		 0
		 (+ 1 (max (walk (node-left node))
			   (walk (node-right node)))))))
    (declare (ftype (function ((or null node)) array-length) walk))
    (walk (interval-table-tree table))))

(defun print-ivt (tab s)
  (format s "#S(INTERVAL-TABLE :LESS ~S :BOUNDS ~S :COUNT ~S :HEIGHT ~S)"
	  (interval-table-less tab)
	  (interval-table-bounds tab)
	  (interval-table-count tab)
	  (height tab)))

	     
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
If not, returns default and nil.
O(log n)."
  (let ((%less (interval-table-less table)))
    (labels ((walk (node)
	       (declare (type (or null node) node))
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


(declaim (ftype (function (interval-table) (values t t t)) get-min get-max get-last))

(defun get-min (table)
  "Get the minimum interval in the table.
Returns three values: lower bound, upper bound, value.
If the table is empty, returns nil for all values.
O(log n)."
  (labels ((walk (h)
	     (declare (type node h))
	     (let ((left (node-left h)))
	       (if (null left)
		   (values (node-lo h) (node-hi h) (node-value h))
		   (walk left)))))
    (let ((tree (interval-table-tree table)))
      (if (null tree)
	  (values nil nil nil)
	  (walk tree)))))

(defun get-max (table)
  "Get the maximum interval in the table.
Returns three values: lower bound, upper bound, value.
If the table is empty, returns nil for all values.
O(log n)."
  (labels ((walk (h)
	     (declare (type node h))
	     (let ((right (node-right h)))
	       (if (null right)
		   (values (node-lo h) (node-hi h) (node-value h))
		   (walk right)))))
    (let ((tree (interval-table-tree table)))
      (if (null tree)
	  (values nil nil nil)
	  (walk tree)))))


(declaim (ftype (function (function node) (values (or null node) node)) %delete-min))
(defun %delete-min (%less node)
  (labels ((del (h)
	     (declare (type node h))
	     (if (null (node-left h))
		 (values nil h)
		 (progn
		   (when (and (not (red-p (node-left h))) (not (red-p (node-left (node-left h)))))
		     (setq h (move-red-left %less h)))
		   (multiple-value-bind (tree min)
		       (del (node-left h))
		     (setf (node-left h) tree)
		     (values (fix-up %less h) min))))))
    (del node)))

(declaim (ftype (function (interval-table) (values t t t)) delete-min))
(defun delete-min (table)
    "Delete the minimum interval from the table.
Returns the deleted interval as three values: lower bound, upper bound, value.
If the table is empty, returns nil for all values.
O(log n)."
    (if (null (interval-table-tree table))
	(values nil nil nil)
	(multiple-value-bind (tree min)
	    (%delete-min (interval-table-less table) (interval-table-tree table))
	  (when tree
	    (setf (node-red-p tree) nil))
	  (setf (interval-table-tree table) tree)
	  (values (node-lo min) (node-hi min) (node-value min)))))


(declaim (ftype (function (t t interval-table) (values t boolean)) delete-interval))
(defun delete-interval (lo hi table)
  "Delete the entry for interval lo,hi in table, if any.
Returns the entries value and true if there was such an entry, or nil and false otherwise.
O(log n)."
  (let ((%less (interval-table-less table)))
    (labels ((del (h)
	       (declare (type node h))
	       (let ((r (compare-interval-to-node %less lo hi h)))
		 (declare (type (integer -1 1) r))
		 (if (< r 0)
		     (progn
		       (when (and (not (red-p (node-left h)))
				  (not (red-p (node-left (node-left h)))))
			 (setq h (move-red-left %less h)))
		       (setf (node-left h) (del (node-left h)))
		       (fix-up %less h))
		     (progn
		       (when (red-p (node-left h))
			 (setq h (rotate-right %less h))
			 (setq r (compare-interval-to-node %less lo hi h)))
		       (if (and (= r 0) (null (node-right h)))
			   nil
			   (progn
			     (when (and (not (red-p (node-right h)))
					(not (red-p (node-left (node-right h)))))
			       (setq h (move-red-right %less h))
			       (setq r (compare-interval-to-node %less lo hi h)))
			     (if (= r 0)
				 (multiple-value-bind (tree min)
				     (%delete-min %less (node-right h))
				   (setf (node-lo h) (node-lo min)
					 (node-hi h) (node-hi min)
					 (node-value h) (node-value min))
				   (setf (node-right h) tree)
				   (fix-up %less h))
				 (progn
				   (setf (node-right h) (del (node-right h)))
				   (fix-up %less h))))))))))
      (multiple-value-bind (value present-p)
	  (get-interval lo hi table)
	(if (not present-p)
	    (values nil nil)
	    (progn
	      (setf (interval-table-tree table) (del (interval-table-tree table)))
	      (values value t)))))))

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


(declaim (ftype (function (function interval-table)) map-values))
(defun map-values (function table)
  "Update all values in table by the result of calling function with the lower bound, upper bound, and value."
  (labels ((walk (node)
	     (declare (type (or null node) node))
	     (when node
	       (setf (node-value node) (funcall function (node-lo node) (node-hi node) (node-value node)))
	       (walk (node-left node))
	       (walk (node-right node)))))
    (walk (interval-table-tree table))
    (values)))

(declaim (ftype (function (interval-table &key (:containing t) (:intersecting list) (:above t) (:below t))
			  interval-table)
		subtable))
(defun subtable (table &key containing intersecting above below)
  (make-interval-table
   (interval-table-less table)
   :bounds (interval-table-bounds table)
   :initial-contents* (map-intervals 'vector #'list* table
				     :containing containing
				     :intersecting intersecting
				     :above above
				     :below below)))

(defun %map-to-list (function table containing from-end)
  (declare (type function function)
	   (type interval-table table))
  (labels ((collect (e xs)
	     (declare (type (or null node) e)
		      (type list xs))
             (cond ((null e) xs)
		   ((and containing (above-upper-bound-p table (node-max-upper e) containing))
		    ;; point is above the maximum upper bound: no result
		    xs)
		   ((and containing (before-node-p table e containing))
		    ;; point is to the left of the node: can't be in right subtree
		    (collect (node-left e) xs))
		   (t
                    (collect
		     (node-left e)
		     (if (or (not containing)
			     (node-contains-point-p table e containing))
			 (cons (funcall function (node-lo e) (node-hi e) (node-value e))
                               (collect (node-right e) xs))
			 (collect (node-right e) xs))))))
	   (collect-from-end (e xs)
	     (declare (type (or null node) e)
		      (type list xs))
             (cond ((null e) xs)
		   ((and containing (above-upper-bound-p table (node-max-upper e) containing))
		    ;; point is above the maximum upper bound: no result
		    xs)
		   ((and containing (before-node-p table e containing))
		    ;; point is to the left of the node: can't be in right subtree
		    (collect-from-end (node-left e) xs))
		   (t
                    (collect-from-end
		     (node-right e)
		     (if (or (not containing)
			     (node-contains-point-p table e containing))
			 (cons (funcall function (node-lo e) (node-hi e) (node-value e))
                               (collect-from-end (node-left e) xs))
			 (collect-from-end (node-left e) xs)))))))
    (if from-end
	(collect-from-end (interval-table-tree table) nil)
	(collect (interval-table-tree table) nil))))

(defun %map-to-vector (element-type function table containing from-end)
  (let ((result (make-array 10 :element-type element-type :fill-pointer 0 :adjustable t)))
    (labels ((add-result (e)
	       (declare (type node e))
	       (vector-push-extend (funcall function (node-lo e) (node-hi e) (node-value e))
				   result
				   (+ 1 (* 2 (length result)))))
	     (walk (e)
	       (declare (type (or null node) e))
	       (cond ((null e) nil)
		     ((and containing (above-upper-bound-p table (node-max-upper e) containing))
		      ;; point is above the maximum upper bound: no result
		      nil)
		     ((and containing (before-node-p table e containing))
		      ;; point is to the left of the node: can't be in right subtree
		      (walk (node-left e)))
		     (t
                      (walk (node-left e))
		      (when (or (not containing)
				(node-contains-point-p table e containing))
			(add-result e))
		      (walk (node-right e)))))
	     (walk-from-end (e)
	       (declare (type (or null node) e))
	       (cond ((null e) nil)
		     ((and containing (above-upper-bound-p table (node-max-upper e) containing))
		      ;; point is above the maximum upper bound: no result
		      nil)
		     ((and containing (before-node-p table e containing))
		      ;; point is to the left of the node: can't be in right subtree
		      (walk-from-end (node-left e)))
		     (t
                      (walk-from-end (node-right e))
		      (when (or (not containing)
				(node-contains-point-p table e containing))
			(add-result e))
		      (walk-from-end (node-left e))))))
      (if from-end
	  (walk-from-end (interval-table-tree table))
	  (walk (interval-table-tree table)))
      result)))

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
  (assert (at-most-one-of containing intersecting above below))
  (cond ((null result-type) (%for-each function table from-end))
        ((eq result-type 'list) (%map-to-list function table containing from-end))
	((eq result-type 'string) (%map-to-vector 'character function table containing from-end))
	((eq result-type 'vector) (%map-to-vector t          function table containing from-end))
	((and (consp result-type) (eq (car result-type) 'vector))
	  (%map-to-vector (if (cdr result-type) (cadr result-type) t) function table containing from-end))
	;; string (string 4711) (string *) vector (vector type [size]) (array type (size))
        (t (error "invalid result-type ~S" result-type))))


(declaim (ftype (function (function interval-table) boolean) every-interval))
(defun every-interval (predicate table)
  "Check that predicate is true for each interval in the table.
Predicate must take three arguments: lower bound, upper bound, value."
  (labels ((walk (e)
	     (declare (type (or null node) e))
	     (or (null e)
		 (and (funcall predicate (node-lo e) (node-hi e) (node-value e))
		      (walk (node-left e))
		      (walk (node-right e))))))
    (walk (interval-table-tree table))))


(declaim (ftype (function (function interval-table &key (:from-end boolean)) t) some-interval))
(defun some-interval (predicate table &key from-end)
  "Return the first non-nil result from predicate applied to the elements of table.
Predicate must take three arguments: lower bound, upper bound, value."
  (labels ((walk (e)
	     (declare (type (or null node) e))
	     (if (null e)
		 nil
		 (or (walk (node-left e))
		     (funcall predicate (node-lo e) (node-hi e) (node-value e))
		     (walk (node-right e)))))
	   (walk-from-end (e)
	     (declare (type (or null node) e))
	     (if (null e)
		 nil
		 (or (walk-from-end (node-right e))
		     (funcall predicate (node-lo e) (node-hi e) (node-value e))
		     (walk-from-end (node-left e))))))
    (if from-end
	(walk-from-end (interval-table-tree table))
	(walk (interval-table-tree table)))))
