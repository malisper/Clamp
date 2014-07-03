;;;; these are several utilities for performing i/o

(defmacro w/infile (var file &body body)
  "Binds var to the input stream from file and will automatically
   close it"
  `(w/file (,var ,file :direction :input) ,@body))

(defmacro w/outfile (var file &body body)
  "Binds var to the output stream from file and will
   automatically close it. 
   WARNING: This will delete the old file if it already exists"
  `(w/file (,var ,file :direction :output :if-exists :supersede) ,@body))

(defmacro w/appendfile (var file &body body)
  "Same as w/outfile ecept appends to the file instead of superseding it"
  `(w/file (,var ,file :direction :output :if-exists :append) ,@body))
