(in-package :asdf-user)

(defsystem "clamp"
  :description "arc functions and macros"
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
		:components ((:file "defalias")
			     (:file "aliases")
			     (:file "base" :depends-on ("aliases"))
			     (:file "binding" :depends-on ("base"))
			     (:file "print" :depends-on ("aliases"))
			     (:file "hof" :depends-on ("aliases" "iter"))
			     (:file "conditions" :depends-on ("binding" "base" "macros"))
			     (:file "list" :depends-on ("binding" "conditions"))
			     (:file "macros" :depends-on ("binding" "hof" "base"))
			     (:file "iter" :depends-on ("binding" "fns"))
			     (:file "fns" :depends-on ("base"))
			     (:file "misc" :depends-on ("macros" "binding" "conditions"))
			     (:file "fnops" :depends-on ("conditions" "binding"))
			     (:file "sort" :depends-on ("base"))
			     (:file "memoize" :depends-on ("binding" "conditions" "macros"))
			     (:file "tables" :depends-on ("binding" "iter" "macros"))))))
