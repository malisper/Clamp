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

(def partition (test xs &key (key #'identity) (start 0))
  "Returns two lists, the first one containing all of the
   elements of xs that pass the test and the second containing
   all of those that don't."
  (loop with f = (testify test)
        for x being the elements of (subseq xs start)
        if (funcall f (funcall key x))
          collect x into pass
        else
          collect x into fail
        finally (return (values pass fail))))
