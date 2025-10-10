# interval-tables ![Continuous Integration](https://github.com/bokesan/interval-tables/actions/workflows/ci.yml/badge.svg)

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
(setf (get-interval 5 15 *table*) "E5")   ; will overwrite "E3"
```

Getting the size and checking if a table is empty:

```lisp
(interval-table-count *table*) => 4
(interval-table-empty-p *table*) => nil
```

Look up intervals:

```lisp
(get-interval 5 15 *table*) => "E5", t
(get-interval 1 4 *table*) => nil, nil
(get-interval 1 4 *table* "Nope") => "Nope", nil
```

Search for intervals containing a point or intersecting a given interval with `map-intervals`:

```lisp
(map-intervals 'list #'list *table* :containing 9)
    => ((1 10 "E1") (5 15 "E5"))
(map-intervals 'vector #'(lambda (lo hi val) val) *table* :intersecting '(13 50))
    => #( "E5" "E2")
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
(defun arg3 (a b c) (declare (ignore a b)) c)
(map-intervals 'list #'arg3 *table*) => ("E4" "E1" "E5" "E2")
(map-intervals 'list #'arg3 *table* :from-end t) => ("E2" "E5" "E1" "E4")
```

As the table is sorted by the intervals, there are also the functions
`get-min` and `get-max` to get the lowest / highest interval, and
`delete-min` to delete and return the lowest interval from the table:

```lisp
(get-min *table*) => 0, 2, "E4"
(delete-min *table*) => 0, 2, "E4"
(get-min *table*) => 1, 10, "E1"
```

---

## API Documentation

This document provides an overview of the exported functions and types of the `interval-tables` Common Lisp library, based on their docstrings.

---

### Types

#### `interval-table`
A sorted table of non-empty intervals to values.

#### `interval-table-p`
Predicate for `interval-table`.

---

### Construction

#### `make-interval-table`
```lisp
(make-interval-table less &key (bounds :closed) initial-contents initial-contents*)
```
**Docstring:**  
Create a new interval table.  
- `LESS` must be a binary predicate that defines a strict total ordering on the set of interval bounds.
- `BOUNDS` must be one of `:CLOSED`, `:OPEN`, `:CLOSED-OPEN`, `:OPEN-CLOSED`. Defaults to `:CLOSED`.
- `INITIAL-CONTENTS` is a list of `(lo hi value)` lists that become the initial elements of the table.
- `INITIAL-CONTENTS*` is a list of `(lo hi . value)` dotted lists that become the initial elements of the table.

---

### Properties

#### `interval-table-bounds`
Returns the boundary type of the interval table.

#### `interval-table-count`
```lisp
(interval-table-count table)
```
**Docstring:**  
The number of entries in the table. O(n).

#### `interval-table-empty-p`
```lisp
(interval-table-empty-p table)
```
**Docstring:**  
Is the table empty? O(1).

---

### Querying

#### `get-interval`
```lisp
(get-interval lo hi table &optional default)
```
**Docstring:**  
Look up the interval with bounds LO, HI in table.  
If the table contains the interval, return its value and true.  
If not, return DEFAULT and false.  
O(log n).

#### `get-min`
```lisp
(get-min table)
```
**Docstring:**  
Get the minimum interval in TABLE.  
Return three values: lower bound, upper bound, value.  
If the table is empty, return three nil values.  
O(log n).

#### `get-max`
```lisp
(get-max table)
```
**Docstring:**  
Get the maximum interval in the table.  
Return three values: lower bound, upper bound, value.  
If the table is empty, return three nil values.  
O(log n).

---

### Insert / Update / Delete

#### `(setf (get-interval ...))`
Set value for an interval in the table.

#### `delete-interval`
```lisp
(delete-interval lo hi table)
```
**Docstring:**  
Delete the entry for interval LO, HI in TABLE, if any.  
Returns the entry's value and true if there was such an entry, or nil and false otherwise.  
O(log n).

#### `delete-min`
```lisp
(delete-min table)
```
**Docstring:**  
Delete the minimum interval from TABLE.  
Return the deleted interval as three values: lower bound, upper bound, value.  
If the table is empty, return three nil values.  
O(log n).

#### `delete-max`
(Delete the maximum interval from TABLE.  
Behavior follows that of `delete-min` for maximum value.)

---

### Search / Iteration / Mapping

#### `do-intervals`
```lisp
(do-intervals (element table &rest options) &body body)
```
**Docstring:**  
Iterate over the entries in table.  
- `ELEMENT` may be a list of three symbols (bound to lower-bound, upper-bound, and value), a list of two symbols (bound to lower and upper bound), or a symbol (bound to the value).
- `OPTIONS` may be the same options as in `map-intervals`.

#### `map-intervals`
```lisp
(map-intervals result-type function table &key containing intersecting above below from-end)
```
**Docstring:**  
Map function over entries of TABLE.  
- `RESULT-TYPE` gives the type of the result sequence, as in the standard function `map`.
- `FUNCTION` is called for each entry with three arguments: the lower bound, upper bound, value.
- Entries are processed in order. If `FROM-END` is true, processed in reverse.
- The entries to include are selected by one of the following keyword arguments:  
  - `:CONTAINING POINT` -- all intervals containing POINT  
  - `:INTERSECTING (LO HI)` -- all intervals intersecting LO, HI  
  - `:ABOVE POINT` -- all intervals with lower bound > point  
  - `:BELOW POINT` -- all intervals with upper bound < point  
- Only one of `:CONTAINING`, `:INTERSECTING`, or `:ABOVE`+`:BELOW` may be used.

#### `map-values`
```lisp
(map-values function table)
```
**Docstring:**  
Update all values in TABLE by the result of calling FUNCTION with the lower bound, upper bound, and value.

#### `subtable`
```lisp
(subtable table &key containing intersecting above below)
```
**Docstring:**  
Create a new interval table containing a subset of the entries in TABLE.  
- Entries to include are selected by one of the following keyword arguments:  
  - `:CONTAINING POINT`  
  - `:INTERSECTING (LO HI)`  
  - `:ABOVE POINT`  
  - `:BELOW POINT`

#### `every-interval`
```lisp
(every-interval predicate table)
```
**Docstring:**  
Check that PREDICATE is true for each interval in TABLE.  
Called with three arguments: lower bound, upper bound, value.

#### `some-interval`
```lisp
(some-interval predicate table &key from-end)
```
**Docstring:**  
Return the first non-nil result from PREDICATE applied to the elements of TABLE.  
Called with three arguments: lower bound, upper bound, value.  
If `FROM-END` is true, entries are processed in reverse order.

---

### Notes

- All interval searching, mapping, and iteration functions support flexible options for selecting subsets of intervals.
- Interval bounds can be `:open`, `:closed`, `:closed-open`, or `:open-closed` for fine-grained control.
