;;;; Utilities for generating statistics for code.

(in-package :clamp)

(def codelines (file)
  "Counts the number of non-blank, non-comment lines in a file."
  (w/infile in file
    (summing test
      (whilet line (read-line :from in :eof nil)
        (test (aand (find #'nonwhite line)
                    (not (is it #\;))))))))

(def codeflat (file)
  "Returns the number of atoms (excluding nil) in the file."
  (w/infile in file
    (len (flat (readall :from in)))))

(def tokcount (files)
  "Counts the number of times every atom (excluding nil) occurs in
   the given list of files."
  (ret counts (table)
    (each f files
      (w/infile in f
        (each token (flat (readall :from in))
          (++ (gethash token counts 0)))))))
