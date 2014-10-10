(defpackage :clamp-experimental-tests
  (:use :clunit :clamp :clamp-experimental)
  (:nicknames :experimental-tests)
  (:shadowing-import-from :clamp-experimental
     :def :defmemo :mac :fn :coerce))

(in-package :clamp-experimental-tests)

(defsuite clamp-experimental ())
