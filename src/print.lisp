;;;; functions for printing

(in-package "CLAMP")

(def pr (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format"
  (map #'princ args)
  (car args))

(def prn (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format with an additional newline"
  (do1 (apply #'pr args)
       (terpri)))
