;;;; These are several utilities for performing i/o.

(in-package :clamp)
(use-syntax :clamp)

(mac w/infile (var file &body body)
  "Binds VAR to the input stream created from FILE and will
   automatically close it when leaving the w/infile."
  `(w/file (,var ,file :direction :input) ,@body))

(mac w/outfile (var file &body body)
  "Binds VAR to the output stream created from FILE and will
   automatically close it when leaving w/outfile.
   WARNING: This will delete the old file if it already exists."
  `(w/file (,var ,file :direction :output :if-exists :supersede) ,@body))

(mac w/appendfile (var file &body body)
  "Equivalent to w/outfile but appends to FILE instead of superseding
   it."
  `(w/file (,var ,file :direction :output :if-exists :append) ,@body))

(def allchars (str)
  "Returns a string of every char from the input stream, STR."
  (tostring
    (whiler c (readc :from str :eof nil) nil
      (writec c))))

(def filechars (name)
  "Returns a string of every char in the file NAME."
  (w/infile in name (allchars in)))

(def readfile (name)
  "Reads all of the expressions in the file NAME and returns a list
   of the results."
  (w/infile s name (drain (read :from s :eof nil))))

(def readfile1 (name)
  "Reads a single expression by the file NAME."
  (w/infile s name (read :from s)))

(def writefile (val file)
  "Writes VAL to FILE."
  ;; For some reason the Arc version moves the file to a temporary
  ;; version first then renames it.
  (w/outfile s file
    (prin1 val s))
  val)
