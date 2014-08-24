(in-package :asdf-user)

(defsystem "clamp"
  :description "Common Lisp with Arc Macros and Procedures"
  :version "0.3"
  :author "malisper"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "defalias" :depends-on ("package"))
                             (:file "aliases"  :depends-on ("defalias"))
                             (:file "read"     :depends-on ("aliases"))
                             (:file "fns"      :depends-on ("aliases"))
                             (:file "print"    :depends-on ("aliases"))
                             (:file "base"     :depends-on ("aliases"))
                             (:file "hof"      :depends-on ("base" "aliases"))
                             (:file "binding"  :depends-on ("hof"))
                             (:file "macros"   :depends-on ("binding"))
                             (:file "list"     :depends-on ("aliases" "macros"))
                             (:file "conditionals" :depends-on ("macros" "list"))
                             (:file "fnops"    :depends-on ("binding" "base" "conditionals"))
                             (:file "misc"     :depends-on ("macros" "conditionals"))
                             (:file "memoize"  :depends-on ("misc"))
                             (:file "strings"  :depends-on ("misc"))
                             (:file "iter"     :depends-on ("hof"))
                             (:file "io"       :depends-on ("iter"))
                             (:file "tables"   :depends-on ("binding" "iter"))))))
