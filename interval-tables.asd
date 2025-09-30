(defsystem "interval-tables"
  :description "Interval tables with fast search for all intervals containing a point or intersecting an interval."
  :version "0.1"
  :author "Christoph Breitkopf <chbreitkopf@gmail.com>"
  :licence "MIT"
  :depends-on ("alexandria")
  :components ((:file "interval-tables")))

(defsystem "interval-tables/tests"
  :depends-on ("fiveam" "check-it" "interval-tables")
  :components ((:file "tests")))
