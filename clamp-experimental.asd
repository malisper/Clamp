(in-package :asdf-user)

;;;; This is a system only for the experimental features of Clamp. If
;;;; you want both the experimental features and the normal ones, you
;;;; will have to require both and handle the conflicts.

(defsystem "clamp-experimental"
  :description "The experimental features of CLAMP"
  :version "0.1"
  :author "malisper"
  :depends-on ("clamp")
  :components ((:module "experimental"
                :components ((:file "package")
                             (:file "destructuring" :depends-on ("package"))
                             (:file "ssyntax" :depends-on ("package" "destructuring"))
                             (:file "ssyntax-defs" :depends-on ("ssyntax"))
                             (:file "coerce" :depends-on ("package"))
                             (:file "lisp1" :depends-on ("package"))
                             (:file "def" :depends-on ("ssyntax" "destructuring"))))))
