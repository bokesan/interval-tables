# interval-tables
Common Lisp interval tables with fast search for all intervals containing a point or intersecting an interval.

**Work-in-progress, not ready for use.**


## Basic usage

Construct a new interval table given a comparison predicate for interval bounds:

```lisp
(defvar *table* (make-interval-table #'<))
```

Put in some values:

```lisp
(setf (get-interval 1 10 *table*) "a value")
(setf (get-interval 5 15 *table*) "another value")
(setf (get-interval 11 20 *table*) "yet another value")
```

Look up intervals:

```lisp
(get-interval 5 15 *table*) => "another value", t
(get-interval 1 4 *table*) => nil, nil
```


## FAQ

*Why is the package name in plural?*

I expect the package to offer multiple implementations of interval tables with differing
characteristics.
