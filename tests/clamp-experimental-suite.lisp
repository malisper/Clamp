(defpackage :clamp-experimental-tests
  (:use :clunit :clamp :clamp-experimental)
  (:import-from :syntax :use-syntax)
  (:nicknames :experimental-tests)
  (:shadowing-import-from :clamp-experimental
     :def :defmemo :defmethod :mac :fn :coerce))

(in-package :clamp-experimental-tests)

(defsuite clamp-experimental ())
