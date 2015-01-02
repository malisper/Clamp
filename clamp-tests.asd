(defsystem "clamp-tests"
  :description "tests for clamp"
  :depends-on ("clunit" "clamp" "clamp-experimental")
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
                                     (:file "setforms-suite")
                                     (:file "tables-suite")
                                     (:file "strings-suite")
                                     (:file "sort-suite")
                                     (:file "io-suite")

                                     (:file "clamp-experimental-suite")
                                     (:file "destructuring-suite")
                                     (:file "coerce-suite")
                                     (:file "ssyntax-suite")
                                     (:file "lisp1-suite")))))
