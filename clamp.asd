(in-package :asdf-user)

(defsystem "clamp"
  :description "arc functions and macros"
  :version "0.1"
  :author "malisper"
  :components ((:module "src"
		:serial t
		:components ((:file "defalias")
			     (:file "aliases")
			     (:file "base")
			     (:file "clamp")))))
