;;;; these are utilities for working with fns

(def compose (&rest fns)
  "Composes the arguments which are functions"
  (lf fns
      (with (fn1 (last1 fns)
	     fns (butlast fns))
	(fn (&rest args)
	  (reduce #'funcall fns
		  :from-end t
		  :initial-value (apply fn1 args))))
      #'identity))

(def flf (&rest funs)
  "Returns a function which applies each 'test' in sequence
   and if it passes the test calls the next function"
  (case (len funs)
    (0 #'identity)
    (1 (car funs))
    (t (withs ((test fun . rest) funs
	       restfun (apply #'flf rest))
	 (fn (&rest a) (lf (apply test a) (apply fun a)
			   (apply restfun a)))))))

(def andf (fn &rest fns)
  "Returns a predicate function which returns true when all of the
   functions passed in as arguments would return true"
  (lf (null fns)
      fn
      (let1 chain (apply #'andf fns)
	(fn (x)
	  (and (funcall fn x) (funcall chain x))))))

(def orf (fn &rest fns)
  "Returns a predicate function which returns true when any of the
   functions passed in as arguments would return true"
  (lf (null fns)
      fn
      (let1 chain (apply #'orf fns)
	(fn (x)
	  (or (funcall fn x) (funcall chain x))))))
