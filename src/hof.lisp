;;;; several higher order functions

(in-package "CLAMP")

(def testify (x)
  "If passed a function, returns it. Otherwise returns a function which
   tests equality for the object passed"
  (if (functionp x) x [iso x _]))

(def rem (test xs &rest args)
  "Version of remove that uses testify"
  (apply #'remove-if (testify test) xs args))

(def keep (test xs &rest args)
  "Version of remove-if-not that uses testify"
  (apply #'remove-if-not (testify test) xs args))

(def mem (test xs &rest args)
  "Version of mem that uses testify"
  (apply #'member-if (testify test) xs args))

(def find (test xs &rest args)
  "Version of find that uses testify"
  (apply #'find-if (testify test) xs args))

(def count (test xs &rest args)
  "Version of count that uses testify"
  (apply #'count-if (testify test) xs args))

(def pos (test xs &rest args)
  "Version of position that uses testify"
  (apply #'position-if (testify test) xs args))

(def mappend (f &rest xss)
  "Joins the results of mapping f over xs"
  (apply #'join (apply #'map f xss)))

(def subst (old new tree)
  "Substitues everything that passes the testified version of old
   with new (which can be a function which is called on the old elt).
   WARNING: traverses treewise so any nils will be tested"
  (with (test (testify old) next (if (functionp new)
				     new
				     (const new)))
    (rec (tree tree)
       (if (atom tree)
	   (if (funcall test tree)
	       (funcall next tree)
	       tree)
	   (cons (recur (car tree))
		 (recur (cdr tree)))))))
