;;;; several higher order functions

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
