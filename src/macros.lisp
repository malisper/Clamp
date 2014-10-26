;;;; Utilities for writing macros.

(in-package :clamp)

(mac w/uniq (names &body body)
  "Binds every symbol in NAMES to a uniq symbol. Then executes BODY."
  (if (consp names)
      `(with ,(mappend (fn (n) `(,n (uniq (symbol-name ',n))))
                       names)
             ,@body)
      `(let ,names (uniq (symbol-name ',names)) ,@body)))

(def mkstr (&rest args)
  "Returns the string representing all of the arguments."
  (tostring
    (apply #'pr args)))

(def symb (&rest args)
  "Returns a symbol representing all of the arguments."
  (values (intern (apply #'mkstr args))))

;; Based on arg-count in PAIP.
(def check-len (name form xs min &key (max nil)
                     (str "Wrong number of arguments for ~A in ~A: ~
                           ~A supplied, ~A~@[ to ~A~] expected."))
  "Asserts that some list, XS, has between MIN and MAX elements. If
   XS does not have the right number of arguments STR is the error
   string with NAME, FORM, (len XS), MIN, MAX as its arguments."
  (let len-xs (len xs)
    (assert (and (<= min len-xs (or max min)))
            () str name form len-xs min max)))
