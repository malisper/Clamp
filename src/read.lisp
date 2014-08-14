(in-package :clamp)

(def readb (&key (from *standard-input*) (eof nil eof-p))
  "Reads a byte from 'from'. If reaches end of the file, signal an
   error if 'eof' was not supplied. If 'eof' was supplied, return 
   it."
  (read-byte from (no eof-p) eof))

(def readc (&key (from *standard-input*) (eof nil eof-p) (recur nil))
  "Reads a char from 'from'. If readc reaches end of the file signal 
   an error if 'eof' was not supplied. If 'eof' was supplied, return
   it. The 'recur' argument is if it is possible this call can lead 
   to another call to read."
  (read-char from (no eof-p) eof recur))

(def peekc (&key (from *standard-input*) (eof nil eof-p)
                 (recur nil) (type nil))
  "Same as readc but leaves the char on the stream. If 'type' is nil
   return the next char. If 'type' is t, return the next char,
   skipping whitespace. Otherwise if 'type' is a char, return the
   next char that is char= to 'type'."
  (peek-char type from (no eof-p) eof recur))

(def read (&key (from *standard-input*) (eof nil eof-p) (recur nil))
  "Same as cl:read but uses keyword arguments."
  (cl:read from (no eof-p) eof recur))

(def read-line (&key (from *standard-input*) (eof nil eof-p) (recur nil))
  "Same as cl:read-line but uses keyword arguments."
  (cl:read-line from (no eof-p) eof recur))
