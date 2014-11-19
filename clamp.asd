(in-package :asdf-user)

(defsystem "clamp"
  :description "Common Lisp with Arc Macros and Procedures"
  :version "0.3"
  :author "malisper"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "defalias" :depends-on ("package"))
                             (:file "aliases"  :depends-on ("defalias"))
                             (:file "fns"      :depends-on ("aliases"))
                             (:file "base"     :depends-on ("aliases"))
                             (:file "read"     :depends-on ("aliases"))
                             (:file "hof"      :depends-on ("aliases" "base"))
                             (:file "binding"  :depends-on ("hof"))
                             (:file "print"    :depends-on ("aliases" "base" "binding" "hof"))
                             (:file "time"     :depends-on ("aliases" "print"))
                             (:file "macros"   :depends-on ("binding" "print"))
                             (:file "conditionals" :depends-on ("macros" "list"))
                             (:file "fnops"    :depends-on ("binding" "base" "conditionals"))
                             (:file "setforms" :depends-on ("binding" "macros"))
                             (:file "memoize"  :depends-on ("setforms"))
                             (:file "strings"  :depends-on ("misc"))
                             (:file "iter"     :depends-on ("hof" "macros"))
                             (:file "list"     :depends-on ("aliases" "macros" "iter" "base"))
                             (:file "misc"     :depends-on ("macros" "conditionals" "iter" "list" "hof"))
                             (:file "sort"     :depends-on ("binding" "list" "iter"))
                             (:file "io"       :depends-on ("iter" "read"))
                             (:file "tables"   :depends-on ("binding" "iter"))
                             (:file "disk"     :depends-on ("macros" "conditionals" "io"))))))
