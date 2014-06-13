(defsystem "clamp_tests"
  :description "tests for clamp"
  :depends-on ("clunit" "clamp")
  :components ((:module "src"
			:components ((:file "test")))))
