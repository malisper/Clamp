;;;; These are utilities for working with strings.

;;;; For most string related operations I suggest using some regex
;;;; based library. Using such a library removes the purpose of most
;;;; of the Arc utilities for string searching.

(in-package :clamp)
(use-syntax :clamp)

(def newstring (length &optional char)
  "Creates a newstring of length LENGTH of the character CHAR."
  ;; The value nil can't be passed as the initial-element.
  (if char
      (make-string length :initial-element char)
      (make-string length)))

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

(def upcase (x)
  "Converts a string or a char to uppercase."
  (typecase x
    string    (string-upcase x)
    character (char-upcase   x)
    symbol    (intern (upcase (symbol-name x)))))

(def downcase (x)
  "Converts a string or a char to lowercase."
  (typecase x
    string    (string-downcase x)
    character (char-downcase   x)
    symbol    (intern (downcase (symbol-name x)))))

(def headmatch (pat seq &optional (start 0))
  "Does SEQ, starting from START, match PAT?"
  (loop for i from 0 below (len pat)
        for j from start
        always (is (elt pat i) (elt seq j))))

(def begins (seq pat &optional (start 0))
  "Equivalent to headmatch, but SEQ and PAT are reversed. Also this
   tests whether SEQ is long enough first."
  (unless (> (len pat) (- (len seq) start))
    (headmatch pat seq start)))

(def ellipsize (str &optional (limit 80))
  "If the length of STR is greater than LIMIT, take the first LIMIT
   characters and append '...' to them."
  (if (<= (len str) limit)
      str
      (mkstr (cut str 0 limit) "...")))
