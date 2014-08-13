(in-package :clamp)

(def whitec (c)
  "Is this character whitespace (a space, newline, tab, or return)?"
  (in c #\space #\newline #\tab #\return))

(def tokens (str &optional (sep #'whitec))
  "Returns a list of containg all of the parts of str separated by
   using sep (a test)."
  (let test (testify sep)
    (loop for prev = 0 then (+ next 1)
          for next = (pos test str :start prev)
          for substr = (cut str prev next)
          ;; there must be a better way to test for this
          unless (iso "" substr)
            collect substr
          while next)))
