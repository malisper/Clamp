(in-package :asdf-user)

(defsystem "clamp"
  :description "arc functions and macros"
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
		:components ((:file "defalias")
			     (:file "aliases")
			     (:file "base")
			     (:file "binding" :depends-on ("base"))
			     (:file "print" :depends-on ("aliases"))
			     (:file "hof" :depends-on ("aliases" "iter"))
			     (:file "conditions" :depends-on ("binding"))
			     (:file "list" :depends-on ("binding" "conditions"))
			     (:file "macros" :depends-on ("binding" "hof" "conditions"))
			     (:file "iter" :depends-on ("binding" "fns"))
			     (:file "fns")
			     (:file "misc" :depends-on ("macros" "binding" "conditions"))
			     (:file "fnops" :depends-on ("conditions" "binding"))
			     (:file "sort" :depends-on ("base"))
			     (:file "memoize" :depends-on ("binding" "conditions" "macros"))
			     (:file "tables" :depends-on ("binding" "iter" "macros"))))))
