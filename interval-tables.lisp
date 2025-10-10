(defpackage :interval-tables
  (:use :cl)
  (:import-from :alexandria :array-length)
  (:export
   ;; === types ===
   :interval-table
   :interval-table-p
   ;; === construction ===
   :make-interval-table
   ;; === properties ===
   :interval-table-bounds
   :interval-table-count
   :interval-table-empty-p
   ;; === querying ===
   :get-interval
   :get-min :get-max
   ;; === insert / update / delete ===
   ;; (setf (get-interval ...) value)
   :delete-interval
   :delete-min
   :delete-max
   ;; === search / iteration / mapping ===
   :do-intervals
   :map-intervals
   :map-values
   :subtable
   :every-interval
   :some-interval))

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


(declaim (ftype (function (function node) node) move-red-left move-red-right))

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

(defstruct (interval-table (:constructor %make-interval-table))
  "A sorted table of non-empty intervals to values."
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

(defmethod print-object ((tab interval-table) s)
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
  "Look up the interval with bounds LO, HI in table.
If the table contains the interval, return its value and true.
If not, return DEFAULT and false.
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
  "Associate the interval LO, HI with VALUE in TABLE. Returns VALUE."
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
  "Get the minimum interval in TABLE.
Return three values: lower bound, upper bound, value.
If the table is empty, return three nil values.
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
Return three values: lower bound, upper bound, value.
If the table is empty, return three nil values.
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
    "Delete the minimum interval from TABLE.
Return the deleted interval as three values: lower bound, upper bound, value.
If the table is empty, return three nil values.
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
  "Delete the entry for interval LO, HI in TABLE, if any.
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
    (case (interval-table-bounds tab)
      ((:closed :closed-open) (%lt point (node-lo node)))
      (t                      (%le point (node-lo node))))))

(defun after-node-p (tab node point)
  (let ((%less (interval-table-less tab)))
    (case (interval-table-bounds tab)
      ((:closed :open-closed) (%gt point (node-hi node)))
      (t                      (%ge point (node-hi node))))))

(defun above-upper-bound-p (tab upper point)
  (let ((%less (interval-table-less tab)))
    (case (interval-table-bounds tab)
      ((:closed :open-closed) (%gt point upper))
      (t                      (%ge point upper)))))

(defun node-contains-point-p (tab node point)
  (let ((%less (interval-table-less tab))
        (lo (node-lo node))
        (hi (node-hi node)))
    (ecase (interval-table-bounds tab)
      (:closed (and (%le lo point) (%le point hi)))
      (:open (and (%lt lo point) (%lt point hi)))
      (:closed-open (and (%le lo point) (%lt point hi)))
      (:open-closed (and (%lt lo point) (%le point hi))))))

(defun overlaps-node-p (tab node lo hi)
  (let ((%less (interval-table-less tab)))
    (if (eq (interval-table-bounds tab) :closed)
	(and (%le lo (node-hi node)) (%ge hi (node-lo node)))
        (and (%lt lo (node-hi node)) (%gt hi (node-lo node))))))

(declaim (ftype (function (function interval-table)) map-values))
(defun map-values (function table)
  "Update all values in TABLE by the result of calling FUNCTION with the lower bound, upper bound, and value."
  (labels ((walk (node)
	     (declare (type (or null node) node))
	     (when node
	       (setf (node-value node) (funcall function (node-lo node) (node-hi node) (node-value node)))
	       (walk (node-left node))
	       (walk (node-right node)))))
    (walk (interval-table-tree table))
    (values)))


(declaim (ftype (function (function interval-table t)) %all)
	 (ftype (function (function interval-table t t)) %containing)
	 (ftype (function (function interval-table t t t t t) list) %map-to-list)
	 (ftype (function (function interval-table t t t t t)) %for-each)
	 (ftype (function ((or symbol list) function interval-table t t t t t) vector) %map-to-vector))

(defun %all (accept table from-end)
  (labels ((walk (e)
	     (declare (type (or null node) e))
	     (when e
	       (walk (if from-end (node-right e) (node-left e)))
	       (funcall accept (node-lo e) (node-hi e) (node-value e))
	       (walk (if from-end (node-left e) (node-right e))))))
    (walk (interval-table-tree table))))

(defun %containing (accept table point from-end)
  (labels ((walk (e)
	     (declare (type (or null node) e))
	     (cond ((null e) nil)
		   ((above-upper-bound-p table (node-max-upper e) point)
		    ;; point is above the maximum upper bound: no result
		    nil)
		   ((before-node-p table e point)
		    ;; point is to the left of the node: can't be in right subtree
		    (walk (node-left e)))
		   (t
                    (walk (if from-end (node-right e) (node-left e)))
		    (when (node-contains-point-p table e point)
		      (funcall accept (node-lo e) (node-hi e) (node-value e)))
		    (walk (if from-end (node-left e) (node-right e)))))))
    (walk (interval-table-tree table))))

(defun %between (accept table from to from-end)
  (labels ((walk (e)
	     (declare (type (or null node) e))
	     (cond ((null e) nil)
		   ((and from (above-upper-bound-p table (node-max-upper e) from))
		    nil)
		   ((and to (before-node-p table e to))
		    (walk (node-left e)))
		   (t
                    (walk (if from-end (node-right e) (node-left e)))
		    (when (and (or (not from) (before-node-p table e from))
			       (or (not to) (after-node-p table e to)))
		      (funcall accept (node-lo e) (node-hi e) (node-value e)))
		    (walk (if from-end (node-left e) (node-right e)))))))
    (walk (interval-table-tree table))))
		    

(defun %intersecting (accept table interval from-end)
  (let ((lo (car interval))
	(hi (cadr interval)))
    (labels ((walk (e)
	       (declare (type (or null node) e))
	       (cond ((null e) nil)
		     ((above-upper-bound-p table (node-max-upper e) lo)
		      nil)
		     ((before-node-p table e hi)
		      (walk (node-left e)))
		     (t
		      (walk (if from-end (node-right e) (node-left e)))
		      (when (overlaps-node-p table e lo hi)
			(funcall accept (node-lo e) (node-hi e) (node-value e)))
		      (walk (if from-end (node-left e) (node-right e)))))))
      (walk (interval-table-tree table)))))

(defun %map-to-list (function table containing intersecting above below from-end)
  (declare (type function function)
	   (type interval-table table))
  (let ((result nil))
    (flet ((accept (lo hi value)
	     (push (funcall function lo hi value) result)))
      (cond (containing
	     (%containing #'accept table containing from-end))
	    (intersecting
	     (%intersecting #'accept table intersecting from-end))
	    ((or above below)
	     (%between #'accept table above below from-end))
	    (t
	     (%all #'accept table from-end)))
      (nreverse result))))

(defun %map-to-vector (element-type function table containing intersecting above below from-end)
  (let ((result (make-array 10 :element-type element-type :fill-pointer 0 :adjustable t)))
    (flet ((add-result (lo hi value)
	     (vector-push-extend (funcall function lo hi value)
				 result
				 (+ 1 (* 2 (length result))))))
      (cond (containing
	     (%containing #'add-result table containing from-end))
	    (intersecting
	     (%intersecting #'add-result table intersecting from-end))
	    ((or above below)
	     (%between #'add-result table above below from-end))
	    (t
	     (%all #'add-result table from-end)))
      result)))

(defun %for-each (function table containing intersecting above below from-end)
  (declare (type function function)
	   (type interval-table table))
  (cond (containing (%containing function table containing from-end))
	(intersecting (%intersecting function table intersecting from-end))
	((or above below) (%between function table above below from-end))
	(t (%all function table from-end))))

(defmacro do-intervals ((element table &rest options) &body body)
  "Iterate over the entries in table.
ELEMENT may be a list of three symbols, which are bound to
the lower-bound, upper-bound, and value of each element in the table,
or a list of just two symbols, which are bound to the lower and upper bound,
or a symbol, which is bound to the value.
OPTIONS may be the same options as in `map-intervals'."
  (let* ((ignore1 (gensym))
	 (ignore2 (gensym))
	 (args+ignores (cond ((symbolp element)
			      (cons (list ignore1 ignore2 element)
				    `((declare (ignore ,ignore1 ,ignore2))
				      (declare (ignorable ,element)))))
			     ((= (length element) 2)
			      (cons (list (car element) (cadr element) ignore1)
				    `((declare (ignore ,ignore1))
				      (declare (ignorable ,(car element) ,(cadr element))))))
			     ((= (length element) 3)
			      (cons element
				    `((declare (ignorable ,@element)))))
			     (t (error "invalid element spec ~S" element)))))
    `(map-intervals nil
		    #'(lambda ,(car args+ignores)
			,@(cdr args+ignores)
			,@body)
		    ,table
		    ,@options)))

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
  "Map function over entries of TABLE.
RESULT-TYPE gives the type of the result sequence, as in the standard function `map'.
FUNCTION is called for each entry with three arguments: the lower bound, upper bound, value.
Entries are processed in order. If FROM-END is true, they are processed in reverse order.
The entries to include are selected by one of the following keyword arguments:
:CONTAINING POINT - all intervals that contain POINT.
:INTERSECTING (LO HI) - all intervals intersecting the interval LO, HI.
:ABOVE POINT - all intervals whose lower bound is strictly greater than point.
:BELOW POINT - all intervals whose upper bound is strictly less than point.
Only one of the options :CONTAINING, :INTERSECTING, or :ABOVE+:BELOW may be used.
That is, :CONTAINING and :INTERSECTING may only be used on their own.
:ABOVE and :BELOW may be used together, but not in combination with :CONTAINING or :INTERSECTING."
  (assert (at-most-one-of containing intersecting (or above below)))
  (cond ((null result-type) (%for-each function table containing intersecting above below from-end))
        ((eq result-type 'list) (%map-to-list function table containing intersecting above below from-end))
	((eq result-type 'string) (%map-to-vector 'character function table containing intersecting above below from-end))
	((eq result-type 'vector) (%map-to-vector t          function table containing intersecting above below from-end))
	((and (consp result-type) (eq (car result-type) 'vector))
	  (%map-to-vector (if (cdr result-type) (cadr result-type) t) function table containing intersecting above below from-end))
	;; string (string 4711) (string *) vector (vector type [size]) (array type (size))
        (t (error "invalid result-type ~S" result-type))))


(declaim (ftype (function (interval-table &key (:containing t) (:intersecting list) (:above t) (:below t))
			  interval-table)
		subtable))
(defun subtable (table &key containing intersecting above below)
  "Create a new interval table containing a subset of the entries in TABLE.
The entries to include are selected by one of the following keyword arguments:
:CONTAINING POINT - all intervals that contain POINT.
:INTERSECTING (LO HI) - all intervals intersecting the interval LO-HI.
:ABOVE POINT - all intervals whose lower bound is strictly greater than point.
:BELOW POINT - all intervals whose upper bound is strictly less than point."
  (make-interval-table
   (interval-table-less table)
   :bounds (interval-table-bounds table)
   :initial-contents* (map-intervals 'vector #'list* table
				     :containing containing
				     :intersecting intersecting
				     :above above
				     :below below)))


(declaim (ftype (function (function interval-table) boolean) every-interval))
(defun every-interval (predicate table)
  "Check that PREDICATE is true for each interval in TABLE.
PREDICATE is called for each entry with three arguments: lower bound, upper bound, value."
  (labels ((walk (e)
	     (declare (type (or null node) e))
	     (or (null e)
		 (and (funcall predicate (node-lo e) (node-hi e) (node-value e))
		      (walk (node-left e))
		      (walk (node-right e))))))
    (walk (interval-table-tree table))))


(declaim (ftype (function (function interval-table &key (:from-end boolean)) t) some-interval))
(defun some-interval (predicate table &key from-end)
  "Return the first non-nil result from PREDICATE applied to the elements of TABLE.
PREDICATE is called for each entry with three arguments: lower bound, upper bound, value.
If FROM-END is true, entries are processed in reverse order."
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
