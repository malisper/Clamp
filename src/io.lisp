;;;; these are several utilities for performing i/o

(in-package :clamp)

(mac w/infile (var file &body body)
  "Binds var to the input stream from file and will automatically
   close it"
  `(w/file (,var ,file :direction :input) ,@body))

(mac w/outfile (var file &body body)
  "Binds var to the output stream from file and will
   automatically close it. 
   WARNING: This will delete the old file if it already exists"
  `(w/file (,var ,file :direction :output :if-exists :supersede) ,@body))

(mac w/appendfile (var file &body body)
  "Same as w/outfile except appends to the file instead of superseding it"
  `(w/file (,var ,file :direction :output :if-exists :append) ,@body))

(def allchars (str)
  "Returns a string of every char from the input stream, str."
  (tostring
    (whiler c (readc :from str :eof nil) nil
      (writec c))))

(def filechars (name)
  "Returns a string of every char in the file named by 'name'."
  (w/infile in name (allchars in)))
