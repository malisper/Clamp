(defsystem "clamp-tests"
  :description "tests for clamp"
  :depends-on ("clunit" "clamp")
  :serial t
  :components ((:module "tests"
                        :components ((:file "clamp-suite")
                                     (:file "base-suite")
                                     (:file "binding-suite")
                                     (:file "conditionals-suite")
                                     (:file "fns-suite")
                                     (:file "fnops-suite")
                                     (:file "hof-suite")
                                     (:file "iter-suite")
                                     (:file "list-suite")
                                     (:file "print-suite")
                                     (:file "memoize-suite")
                                     (:file "misc-suite")
                                     (:file "tables-suite")))))
