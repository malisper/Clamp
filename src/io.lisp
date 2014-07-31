;;;; these are several utilities for performing i/o

(in-package "CLAMP")

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
