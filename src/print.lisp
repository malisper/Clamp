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

(mac w/outstring (var &rest body)
  "Creates a stream 'var' and returns all of the output to var"
  `(with-output-to-string (,var)
     ,@body))

(mac tostring (&body body)
  "Collects all of the output to *standard-output* into a string"
  `(w/outstring *standard-output* ,@body))

(mac w/instring (var string &rest body)
  "Creates an input stream which reads from string"
  `(with-input-from-string (,var ,string)
     ,@body))

(mac fromstring (string &body body)
  "Binds the input from *standard-input to the string"
  `(w/instring *standard-input* ,string ,@body))
