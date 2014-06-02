;;;; several higher order functions

(def testify (x)
  "If passed a function, returns it. Otherwise returns a function which
   tests equality for the object passed"
  (if (functionp x) x [iso x _]))

(def del (test xs &rest args)
  "Version of remove that uses testify"
  (apply #'remove-if (testify test) xs args))

(def keep (test xs &rest args)
  "Version of remove-if-not that uses testify"
  (apply #'remove-if-not (testify test) xs args))

(def mem (test xs &rest args)
  "Version of mem that uses testify"
  (apply #'member-if (testify test) xs args))

(def fnd (test xs &rest args)
  "Version of find that uses testify"
  (apply #'find-if (testify test) xs args))

(def cnt (test xs &rest args)
  "Version of count that uses testify"
  (apply #'count-if (testify test) xs args))

(def pos (test xs &rest args)
  "Version of position that uses testify"
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
