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
(setf (get-interval 1 10 *table*) "E1")
(setf (get-interval 11 20 *table*) "E2")
(setf (get-interval 5 15 *table*) "E3")
(setf (get-interval 0 2 *table*) "E4")
```

Look up intervals:

```lisp
(get-interval 5 15 *table*) => "another value", t
(get-interval 1 4 *table*) => nil, nil
```

Search for intervals containing a point or intersecting a given interval with `map-intervals`:

```lisp
(map-intervals 'list #'list *table* :containing 9)
    => ((1 10 "E1") (5 15 "E3"))
(map-intervals 'vector #'(lambda (lo hi val) val) *table* :intersecting '(13 50))
    => #("E3" "E2")
```

Or you can use `do-intervals`:

```lisp
(do-intervals (((lo hi) value) *table* :containing 0)
  (format t "~A~%" value))
```
will print
```
E4
```

Both `map-intervals` and `do-intervals` process the entries in ascending order.
You can use `:from-end t` to use descending order instead.
If you do not use `:containing` or `:intersecting`, all intervals in the table will be processed:

```lisp
(map-intervals 'list #'(lambda (a b c) c) *table*) => ("E4" "E1" "E3" "E2")
```

## FAQ

*Why is the package name in plural?*

I expect the package to offer multiple implementations of interval tables with differing
characteristics.
