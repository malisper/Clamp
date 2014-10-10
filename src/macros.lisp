;;;; Utilities for writing macros.

(in-package :clamp)

(mac w/uniq (names &body body)
  "Binds every symbol in NAMES to a uniq symbol. Then executes BODY."
  (if (consp names)
      `(with ,(mappend (fn (n) `(,n (uniq (symbol-name ',n))))
                       names)
             ,@body)
      `(let ,names (uniq (symbol-name ',names)) ,@body)))

(def mkstr (&rest args)
  "Returns the string representing all of the arguments."
  (tostring
    (apply #'pr args)))

(def symb (&rest args)
  "Returns a symbol representing all of the arguments."
  (values (intern (apply #'mkstr args))))
