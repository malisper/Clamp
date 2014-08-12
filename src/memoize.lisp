;;;; these are utilities for taking advantage of memoization

(in-package :clamp)

(def memo (f)
  "Returns a memoized version of the function f"
  (let cache (table :test #'iso)
    (fn (&rest args)
        (aif2 (gethash args cache)
              it
              (= (gethash args cache)
                 (apply f args))))))

(mac defmemo (name args &body body)
  "Defines a memoized function."
  (let fn-body `(memo (fn ,args (block ,name ,@body)))
    (w/uniq args
      `(do (defun ,name (&rest ,args) (apply ,fn-body ,args))
           (= (symbol-function ',name) ,fn-body)
           ,(when (stringp (car body))
                  `(= (documentation ',name 'function) ,(car body)))
           ',name))))
