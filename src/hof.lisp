;;;; Higher order funtions.

(in-package :clamp)

(def testify (x &optional (test #'iso))
  "If passed a function, returns it. Otherwise returns a function which
   tests equality for the object passed."
  (if (functionp x) x [funcall test x _]))

(def rem (test xs &rest args)
  "Equivalent to remove-if but 'testifies' TEST first."
  (apply #'remove-if (testify test) xs args))

(def keep (test xs &rest args)
  "Equivalent to remove-if-not but 'testifies' TEST first."
  (apply #'remove-if-not (testify test) xs args))

(def mem (test xs &rest args)
  "Equivalent to member-if but 'testifies' TEST first."
  (apply #'member-if (testify test) xs args))

(def find (test xs &rest args)
  "Equivalent to find-if but 'testifies' TEST first."
  (apply #'find-if (testify test) xs args))

(def count (test xs &rest args)
  "Equivalent to count-if but 'testifies' TEST first."
  (apply #'count-if (testify test) xs args))

(def pos (test xs &rest args)
  "Equivalent to position-if but 'testifies' TEST first."
  (apply #'position-if (testify test) xs args))

(def mappend (f &rest xss)
  "Equivalent to map but appends the results instead of just
   returning them."
  (apply #'join (apply #'map f xss)))

(def partition (test seq &key (key #'identity) (start 0))
  "Returns two lists, the first one containing all of the elements of
   XS that pass the 'testified' version of test and the second 
   containing all of those that don't."
  (loop with f = (testify test)
        for x in (cut (coerce seq 'list) start)
       
        if (funcall f (funcall key x))
          collect x into pass
        else
          collect x into fail
        finally (return (values pass fail))))

(def trues (f xs)
  "Maps F over XS and returns a list of the non-nil results."
  (keep #'idfn (map f xs)))

(mac mapeach (var xs &body body)
  "Executes BODY repetitively with each element of XS bound to VAR.
   Returns a list of the results."
  `(map (fn (,var) ,@body) ,xs))

(mac mappendeach (var xs &body body)
  "Executes BODY repetitively with each element of XS bound to VAR.
   Returns a list of all of the results appended together."
  `(mappend (fn (,var) ,@body) ,xs))

(def positions (test seq)
  "Returns a list of all of the positions of elements in SEQ that 
   pass TEST."
  ;; The macros 'accum' and 'on' are not defined yet.
  (loop with f = (testify test)
        for x in (coerce seq 'list)
        for i from 0
        if (funcall f x)
          collect i))
