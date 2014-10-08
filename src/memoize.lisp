;;;; These are utilities for taking advantage of memoization.

(in-package :clamp)

(def memo (f)
  "Returns a memoized version of the procedure F."
  (let cache (table :test #'iso)
    (fn (&rest args)
      (or2= (gethash args cache) (apply f args)))))

(mac defmemo (name args &body body)
  "Defines a memoized procedure."
  (let fn-body `(memo (fn ,args (block ,name ,@body)))
    (w/uniq args
      `(do (defun ,name (&rest ,args) (apply ,fn-body ,args))
           (= (symbol-function ',name) ,fn-body)
           ,(when (stringp (car body))
              `(= (documentation ',name 'function) ,(car body)))
           ',name))))
