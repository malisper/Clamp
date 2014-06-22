;;;; these are utilities for working with fns

(in-package "CLAMP")

(def compose (&rest fns)
  "Composes the arguments which are functions"
  (if fns
      (with (fn1 (last1 fns)
	     fns (butlast fns))
	(fn (&rest args)
	  (reduce #'funcall fns
		  :from-end t
		  :initial-value (apply fn1 args))))
      #'identity))

(def fif (&rest funs)
  "Returns a function which applies each 'test' in sequence
   and if it passes the test calls the next function"
  (case (len funs)
    0 #'identity
    1 (car funs)
    t (withs ((test fun . rest) funs
	      restfun (apply #'fif rest))
	(fn (&rest args) (if (apply test args)
			     (apply fun args)
			     (apply restfun args))))))

(def andf (f &rest fns)
  "Returns a predicate function which returns true when all of the
   functions passed in as arguments would return true"
  (if (null fns)
      f
      (let chain (apply #'andf fns)
	(fn (x)
	  (and (funcall f x) (funcall chain x))))))

(def orf (f &rest fns)
  "Returns a predicate function which returns true when any of the
   functions passed in as arguments would return true"
  (if (null fns)
      f
      (let chain (apply #'orf fns)
	(fn (x)
	  (or (funcall f x) (funcall chain x))))))

(def curry (f &rest args1)
  "Returns a function with its left most arguments passed in and waiting for the rest"
  (fn (&rest args2) (apply f (append args1 args2))))

(def rcurry (f &rest args1)
  "Returns a function with its right most arguments passed in and waiting for the rest"
  (fn (&rest args2) (apply f (append args2 args1))))
