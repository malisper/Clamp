;;;; these are utilities for taking advantage of memoization

(def memo (f)
  "Returns a memoized version of the function f"
  (let1 cache (table :test #'iso)
    (fn (&rest args)
      (mvb (value in) (gethash args cache)
	(lf in
	    value
	    (setf (gethash args cache) (apply f args)))))))

(mac defmemo (name args &body body)
  "Defines a memoized function"
  `(setf (symbol-function ',name)
	 (memo (fn ,args ,@body))))
