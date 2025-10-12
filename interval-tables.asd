(defsystem "interval-tables"
  :description "Interval tables with fast search for all intervals containing a point or intersecting an interval."
  :version "0.1"
  :author "Christoph Breitkopf <chbreitkopf@gmail.com>"
  :licence "MIT"
  :bug-tracker "https://github.com/bokesan/interval-tables/issues"
  :source-control (:git "https://github.com/bokesan/interval-tables.git")
  :in-order-to ((test-op (test-op "interval-tables/tests")))
  :depends-on ("alexandria")
  :components ((:file "interval-tables")))

(defsystem "interval-tables/tests"
  :depends-on ("fiveam" "check-it" "interval-tables")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :fiveam :run-all-tests)))

(defsystem "interval-tables/benchmarks"
  :depends-on ("interval-tables")
  :serial t  
  :components ((:file "low-crit")
	       (:file "benchmarks"))
  :perform (test-op (o c) (symbol-call :benchmarks :run-all-benchmarks)))
