;;;; these are utilities for taking advantage of memoization

(in-package "CLAMP")

(def memo (f)
  "Returns a memoized version of the function f"
  (let cache (table :test #'iso)
    (fn (&rest args)
      (aif2 (gethash args cache)
	    it
	    (= (gethash args cache)
	       (apply f args))))))

(mac defmemo (name args &body body)
  "Defines a memoized function"
  `(do (= (symbol-function ',name)
	  (memo (fn ,args (block ,name ,@body))))
       ,(when (stringp (car body)) ; test for a documentation string
	  `(= (documentation ',name 'function) ,(car body)))
       ',name))
