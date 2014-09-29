;;;; These are utilities for working with functions.

(in-package :clamp)

(def compose (&rest fns)
  "Composes functions. For example 

   (compose #'1+ #'length) 

   will return a function which returns one plus the length of a 
   list."
  (if fns
      (with (fn1 (last1 fns)
	     fns (butlast fns))
	(fn (&rest args)
	  (reduce #'call fns
		  :from-end t
		  :initial-value (apply fn1 args))))
      #'identity))

(def fif (&rest funs)
  "Takes in functions, every two of which belong to a pair where the
   first is a predicate, and the second is the consequent function 
   (if there are an odd number of functions, the last one can be 
   thought of as an 'else' function). This returns a function which 
   will apply every test in sequence and if a test returns non-nil, 
   apply the corresponding consequent function. If none of the 
   predicates return non-nil, the function is equivalent to the
   identity function. As an example 

   (fif #'odd #'1+ #'1-) 

   will return a function which will increment odd numbers and
   decrement all other numbers."
  (case (len funs)
    0 #'idfn
    1 (car funs)
    t (withs ((test fun . rest) funs
	      restfun (apply #'fif rest))
	(fn (&rest args) (if (apply test args)
			     (apply fun args)
			     (apply restfun args))))))

(def andf (f &rest fns)
  "Returns a function which lazily applies each function in sequence
   and returns whatever the last function would return if all of the
   other functions return non-nil. For example 

   (andf #'integerp #'even #'1+) 

   will return a function which increments even integers, and
   returns nil for anything else."
  (if (null fns)
      f
      (let chain (apply #'andf fns)
	(fn (&rest args)
	  (and (apply f args) (apply chain args))))))

(def orf (f &rest fns)
  "Returns a function which lazily applies each function in sequence
   and returns the result of the first function that returns a
   non-nil value. For example

   (orf #'odd #'zero) 

   will return a function which tests for an odd number or zero."
  (if (null fns)
      f
      (let chain (apply #'orf fns)
        (fn (&rest args)
          (or (apply f args) (apply chain args))))))

(def curry (f &rest args1)
  "Curries F from the left with the other arguments. For example

   (curry #'reduce #'+)
   
   returns a function which will sum a sequence."
  (fn (&rest args2) (apply f (append args1 args2))))

(def rcurry (f &rest args1)
  "Curries F from the right with the other arguments. For example
   
   (rcurry #'map (range 1 100))

   returns a function which will call its argument on all of the
   numbers from 1 to 100 and collect the results."
  (fn (&rest args2) (apply f (append args2 args1))))

(def flip (f)
  "Returns a new procedure which is the same as F but has its
   arguments in the reverse order."
  (fn (&rest args) (apply f (rev args))))
