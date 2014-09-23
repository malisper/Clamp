;;;; These are utilities that make it easier to read input.

(in-package :clamp)

(def readb (&key (from *standard-input*) (eof nil eof-p))
  "Reads a byte from FROM. If this reaches the end of the file, 
   it signals error if EOF was not supplied. If EOF was supplied, 
   returns EOF."
  (read-byte from (no eof-p) eof))

(def readc (&key (from *standard-input*) (eof nil eof-p) (recur nil))
  "Reads a char from FROM. If this reaches the end of the file signal
   an error if EOF was not supplied. If EOF was supplied, return EOF.
   The RECUR argument is if it is possible this call can lead to
   another call to some version of read."
  (read-char from (no eof-p) eof recur))

(def peekc (&key (from *standard-input*) (eof nil eof-p)
                 (recur nil) (type nil))
  "Same as readc but leaves the char on the stream. If TYPE is nil
   return the next char. If TYPE is t, return the next char after
   skipping whitespace. Otherwise if TYPE is a char, return the
   next char that is char= to TYPE."
  (peek-char type from (no eof-p) eof recur))

(def read (&key (from *standard-input*) (eof nil eof-p) (recur nil))
  "Same as cl:read but uses keyword arguments."
  (cl:read from (no eof-p) eof recur))

(def read-line (&key (from *standard-input*) (eof nil eof-p) (recur nil))
  "Same as cl:read-line but uses keyword arguments."
  (cl:read-line from (no eof-p) eof recur))

(def readfile (name)
  "Reads all of the expressions in the file NAME and returns a list
   of the results."
  (w/infile s name (drain (read :from s :eof nil))))
