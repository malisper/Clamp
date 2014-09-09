;;;; These are utilities for working with strings.

(in-package :clamp)

(def newstring (length &optional char)
  "Creates a newstring of length LENGTH of the character CHAR."
  (make-string length :initial-element char))

(def whitec (c)
  "Is this character whitespace (a space, newline, tab, or return)?"
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c)
  "Is this character not a whitespace character?"
  (no (whitec c)))

(def punc (c)
  "Is this character punctuation?"
  (in c #\. #\, #\; #\: #\! #\?))

(def tokens (str &optional (sep #'whitec))
  "Returns a list of containg all of the parts of str separated by
   using sep (a test)."
  (let test (testify sep)
    (loop for prev = 0 then (+ next 1)
          for next = (pos test str :start prev)
          for substr = (cut str prev next)
          ;; There must be a better way to test for the empty string.
          unless (is 0 (len substr))
            collect substr
          while next)))
