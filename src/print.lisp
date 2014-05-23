;;;; functions for printing

(def pr (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format"
  (mapf #'princ args)
  (car args))

(def prn (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format with an additional newline"
  (prog1 (apply #'pr args)
         (terpri)))
