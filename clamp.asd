(in-package :asdf-user)

(defsystem "clamp"
  :description "arc functions and macros"
  :version "0.2"
  :author "malisper"
  :components ((:module "src"
		:components ((:file "package")
			     (:file "defalias" :depends-on ("package"))
			     (:file "aliases" :depends-on ("defalias"))
			     (:file "base" :depends-on ("aliases"))
			     (:file "binding" :depends-on ("base"))
			     (:file "print" :depends-on ("aliases"))
			     (:file "hof" :depends-on ("aliases" "iter"))
			     (:file "conditionals" :depends-on ("binding" "base" "macros"))
			     (:file "list" :depends-on ("binding" "conditionals"))
			     (:file "macros" :depends-on ("binding" "hof" "base"))
			     (:file "iter" :depends-on ("binding" "fns"))
			     (:file "fns" :depends-on ("base"))
			     (:file "misc" :depends-on ("macros" "binding" "conditionals"))
			     (:file "fnops" :depends-on ("conditionals" "binding"))
			     (:file "sort" :depends-on ("base"))
			     (:file "memoize" :depends-on ("binding" "conditionals" "macros"))
			     (:file "tables" :depends-on ("binding" "iter" "macros"))))))
