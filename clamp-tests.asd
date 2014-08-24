(defsystem "clamp-tests"
  :description "tests for clamp"
  :depends-on ("clunit" "clamp")
  :components ((:module "src"
		:components ((:file "test")))))
