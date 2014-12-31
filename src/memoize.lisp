;;;; These are utilities for taking advantage of memoization.

(in-package :clamp)

(def memo (f)
  "Returns a memoized version of the procedure F."
  (let cache (table :test #'iso)
    (fn (&rest args)
      (or2= (gethash args cache) (apply f args)))))

(mac defmemo (name args &body body)
  "Defines a memoized procedure."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (= (symbol-function ',name) (memo (fn ,args (block ,name ,@body))))
     (declaim (ftype (function (&rest t) t) ,name))
     ,(when (stringp (car body))
            `(= (documentation ',name 'function) ,(car body)))
     ',name))
