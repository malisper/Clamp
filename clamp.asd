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
                             (:file "print"    :depends-on ("aliases" "base"))
                             (:file "hof"      :depends-on ("aliases" "base"))
                             (:file "binding"  :depends-on ("hof"))
                             (:file "macros"   :depends-on ("binding"))
                             (:file "conditionals" :depends-on ("macros" "list"))
                             (:file "fnops"    :depends-on ("binding" "base" "conditionals"))
                             (:file "misc"     :depends-on ("macros" "conditionals"))
                             (:file "memoize"  :depends-on ("misc"))
                             (:file "strings"  :depends-on ("misc"))
                             (:file "iter"     :depends-on ("hof"))
                             (:file "list"     :depends-on ("aliases" "macros" "iter" "base"))
                             (:file "sort"     :depends-on ("binding" "list" "iter"))
                             (:file "io"       :depends-on ("iter" "read"))
                             (:file "tables"   :depends-on ("binding" "iter"))))))
