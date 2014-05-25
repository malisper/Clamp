;;;; several higher order functions

(def testify (x)
  "If passed a function, returns it. Otherwise returns a function which
   tests equality for the object passed"
  (if (functionp x) x [iso x _]))

(def del (test xs &rest args)
  "Deletes all of the elements that pass test in the list xs. If test
   is a function all objects passing the test are removed. If test is
   anything else, all objects iso with it are removed
   NOTE: note the same as delete, as del is not a destructive function"
  (apply #'remove-if (testify test) xs args))

(def keep (test xs &rest args)
  "Same as del but keeps the elements that pass the test"
  (apply #'remove-if-not (testify test) xs args))

(def mem (test xs &rest args)
  "Analog for del and keep but for member"
  (apply #'member-if (testify test) xs args))

(def fnd (test xs &rest args)
  "Analog for del and keep but for find"
  (apply #'find-if (testify test) xs args))

(def pos (test xs &rest args)
  "Analog for del and keep but for position"
  (apply #'position-if (testify test) xs args))

(def mappend (f &rest xss)
  "Joins the results of mapping f over xs"
  (apply #'join (apply #'mapf f xss)))

(def alter (old new seq)
  "Substitues everything passes the testified version of old
   with new (which can be a function)"
  (with (test (testify old) next (if (functionp new)
				     new
				     (const new)))
    (rec (tree seq)
       (if (atom tree)
	   (if (funcall test tree)
	       (funcall next tree)
	       tree)
	   (cons (recur (car tree))
		 (recur (cdr tree)))))))
