(defpackage :interval-tables
  (:use :cl)
  (:export :interval-table :make-interval-table :interval-table-p
	   :interval-table-count
	   :get-interval :rem-interval
	   :do-intervals :map-intervals))

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

(defun compare-entries (less a b)
  (declare (optimize speed))
  (let ((r (compare less (node-lo a) (node-lo b))))
    (if (/= r 0)
	r
	(compare less (node-hi a) (node-hi b)))))

(defun compare-interval-to-node (less lo hi node)
  (declare (optimize speed))
  (let ((r (compare less lo (node-lo node))))
    (if (/= r 0)
	r
	(compare less hi (node-hi node)))))

(defun insert (less tree lo hi value)
  (labels ((ins (e)
	     (declare (optimize speed))
	     (if (null e)
		 (make-node :lo lo :hi hi :value value
			       :max-upper hi)
		 (let ((r (compare-interval-to-node less lo hi e)))
		   (cond ((< r 0) (setf (node-left e) (ins (node-left e))))
			 ((> r 0) (setf (node-right e) (ins (node-right e))))
			 (t (setf (node-value e) value)))
                  (setf (node-max-upper e) (max (node-max-upper e) hi))
                  e))))
    (ins tree)))


(declaim (inline %make-interval-table interval-table-less interval-table-bounds
		 interval-table-tree interval-tree-read-only))

(defstruct (interval-table (:constructor %make-interval-table))
  (less nil :type (function (t t) boolean) :read-only t)
  (bounds :closed :type interval-bounds :read-only t)
  (tree nil :type (or null node))
  (read-only nil :type boolean :read-only t))

#+SBCL (declaim (sb-ext:freeze-type interval-table))

(defun interval-table-count (table)
  (labels ((sum (n node)
	     (declare (type (unsigned-byte 50) n))
	     (if (null node)
		 n
		 (sum (sum (+ n 1) (node-left node)) (node-right node)))))
    (sum 0 (interval-table-tree table))))

(declaim
 (ftype (function ((or symbol (function (t t) boolean))
		   &key
		   (:bounds interval-bounds)
		   (:initial-contents sequence)
		   (:initial-contents* sequence)
		   (:read-only boolean))
		  interval-table)
	make-interval-table))

(defun make-interval-table (less &key
				   (bounds :closed)
				   initial-contents
				   initial-contents*
				   read-only)
  (%make-interval-table
   :less less
   :bounds bounds
   :read-only read-only
   :tree (reduce #'(lambda (tree e)
		     (destructuring-bind (lo hi value) e
		       (insert less tree lo hi value)))
		 initial-contents
		 :initial-value
		 (reduce #'(lambda (tree e)
			     (destructuring-bind (lo hi &rest value) e
			       (insert less tree lo hi value)))
			 initial-contents*
			 :initial-value nil))))


(defun get-interval (table lo hi &optional default)
  (let ((%less (interval-table-less table)))
    (labels ((walk (node)
	       (if (null node)
		   (values default nil)
		   (let ((r (compare-interval-to-node %less lo hi node)))
		     (cond ((< r 0) (walk (node-left node)))
			   ((> r 0) (walk (node-right node)))
			   (t (values (node-value node) t)))))))
      (walk (interval-table-tree table)))))

(defun empty-interval-p (table lo hi)
  (let ((%less (interval-table-less table)))
    (if (eq (interval-table-bounds table) :closed)
	(%lt hi lo)
	(%le hi lo))))

(defun interval-table-put (table lo hi value)
  (when (empty-interval-p table lo hi)
    (error "empty interval ~S ~S ~S" lo hi (interval-table-bounds table)))
  (setf (interval-table-tree table)
	(insert (interval-table-less table)
		(interval-table-tree table)
		lo hi value))
  value)

(defsetf get-interval (table lo hi &optional default) (new-value)
  `(interval-table-put ,table ,lo ,hi ,new-value))

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
  
(defun %map-to-list (function table)
  (labels ((collect (e xs)
             (if (null e)
                 xs
                 (collect (node-left e)
                          (cons (funcall function (node-lo e) (node-hi e) (node-value e))
                                (collect (node-right e) xs))))))
    (collect (interval-table-tree table) nil)))

(defun %for-each (function table)
  (labels ((walk (e)
             (when e
               (walk (node-left e))
               (funcall function (node-lo e) (node-hi e) (node-value e))
               (walk (node-right e)))))
    (walk (interval-table-tree table))))

(defmacro do-intervals (((lo hi) value table) &body body)
  (let ((proc (gensym))
        (e (gensym))
        (tab (gensym)))
    `(let ((,tab ,table))
       (labels ((,proc (,e)
                (when ,e
                  (,proc (node-left ,e))
                  (let ((,lo (node-lo ,e))
                        (,hi (node-hi ,e))
                        (,value (node-value ,e)))
                    (declare (ignorable ,lo ,hi ,value))
                    ,@body)
                  (,proc (node-right ,e)))))
       (,proc (interval-table-tree ,tab))))))

(declaim (ftype (function (t function interval-table) t) map-intervals))

(defun map-intervals (result-type function table &key containing intersecting above below)
  (cond ((null result-type) (%for-each function table))
        ((eq result-type 'list) (%map-to-list function table))
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

