;;;; Utilities for printing.

(in-package :clamp)

(def pr (&rest args)
  "Prints all of its arguments to *standard-output* in a human 
   readable format."
  (map #'princ args)
  (car args))

(def prn (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format with an additional newline."
  (do1 (apply #'pr args)
       (terpri)))

(def prf (control-string &rest args)
  "Equivalent to format but automatically prints to *standard-output*."
  (apply #'format t control-string args))

(def prs (&rest args)
  "Prints the arguments seperated by spaces and returns the arguments
   as a list."
  (prf "~{~A~^ ~}" args)
  args)

(mac w/outstring (var &rest body)
  "Creates a string output stream and binds it to VAR."
  `(with-output-to-string (,var)
     ,@body))

(mac tostring (&body body)
  "Collects all of the output to *standard-output* into a string."
  `(w/outstring *standard-output* ,@body))

(mac w/instring (var string &rest body)
  "Binds an string input stream which reads from STRING, to VAR."
  `(with-input-from-string (,var ,string)
     ,@body))

(mac fromstring (string &body body)
  "Makes the input from *standard-input* read from STRING."
  `(w/instring *standard-input* ,string ,@body))

(mac tofile (name &body body)
  "Redirects *standard-output* to the file NAME.
   WARNING: supersedes the file."
  `(w/outfile *standard-output* ,name
     ,@body))

(mac fromfile (name &body body)
  "Makes the input from *standard-input* read from the file NAME."
  `(w/infile *standard-input* ,name
     ,@body))

(def sp (&optional (n 1))
  "Prints the given number of spaces."
  (loop repeat n
        do (pr " ")))
