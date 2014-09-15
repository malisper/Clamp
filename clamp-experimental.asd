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
                             (:file "ssyntax" :depends-on ("package"))
                             (:file "destructuring" :depends-on ("package"))
                             (:file "coerce" :depends-on ("package"))))))
