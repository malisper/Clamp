;;;; This is an experimental implementation of argument destructuring.

(in-package :experimental)
(use-syntax :clamp)

;;;; Right now this implements a basic form of argument destructuring
;;;; and allows shorter names for the different kinds of arguments (?
;;;; for optional arguments, ! for keyword arguments, and an improper
;;;; argument list is equivalent to one that uses &rest [scheme
;;;; style]). One can even use a symbol to represent all of the
;;;; arguments. For example (fn args (reduce #'+ args)) is a procedure
;;;; which sums its arguments. Even cooler is (fn fn fn) which is
;;;; equivalent to the list procedure. To use destructuring with
;;;; optional arguments, they needs to be surrounded by parens,
;;;; otherwise it is ambigous as to whether (x a) meas that x
;;;; defaults to the value of a, or this is meant to destructure
;;;; into variables x and a.

;;;; Issues
;;;; Keyword arguments do not work with destructuring. What would the
;;;; keyword argument be? Even if one provided a name with the
;;;; argument the syntax would be pretty complicated to represent it.
;;;; Also any arguments named by ? or ! will not work. This can be
;;;; fixed by modifying parse-arguments to work recursively up the
;;;; argument list ignoring the variable name. Then switching the use
;;;; of sublis in add-keywords with subst-list.

(defun subst-list (alist xs)
  "Substitutes the corresponding values in ALIST into the list XS."
  (map [aif2 (alref alist _) it _] xs))

(defun last-atom (xs)
  "Returns the last atom of an improper list and everything that
   occurs before it."
  (rec (cur xs acc '())
    (if (atom cur)
        (values cur (rev acc))
        (recur (cdr cur) (cons (car cur) acc)))))

(defun add-keywords (args)
  "Converts symbols such as '?' to their corresponding lambda list
   keyword and adds &rest if the lambda list is not a proper list."
  ;; We first add the &rest if it is needed, then we substitute all
  ;; of the new keywords with the old ones.
  (sublis '((? . &optional)
            (! . &key))
          (add-rest args)))

(defun add-rest (args)
  "If this arglist is an improper list, convert it into one that uses
   &rest."
  (check args
         #'proper
         (mvb (var rest) (last-atom args)
           ;; If this is not a proper list, we want to take whatever
           ;; is in the tail and add a &rest before it.
           (append rest (list '&rest var)))))

(defun parse-args (args)
  "Parses an entire argslist and returns a new argslist, along with an
   alist of arguments that need to be destructured."
  (withs (key-args (add-keywords args)
          pos (pos [mem _ lambda-list-keywords] key-args))
    (if (null pos)
        (parse-normal key-args)
        (mvb (new-args1 alist1) (parse-normal (cut key-args 0 pos))
          (mvb (new-args2 alist2) (parse-optional (cut key-args (+ pos 1)))
            (values (append new-args1 (list (elt key-args pos)) new-args2)
                    (append alist1 alist2)))))))

(defun parse-normal (args)
  "This parses normal arguments in an argslist."
  (loop for arg in args
        for g = (uniq)
        if (consp arg)
          collect g into new-args
          and collect (list g arg) into alist
        else
          collect arg into new-args
        finally (return (values new-args alist))))

(defun parse-optional (args)
  "This parses the optional and keyword arguments in an args."
  (loop for arg in args
        for g = (uniq)
        if (and (consp arg) (consp (car arg)))
          collect (cons g (cdr arg)) into new-args
          and collect (list g (car arg)) into alist
        else
          collect arg into new-args
        finally (return (values new-args alist))))

(defmacro fn (args &body body)
  "Same as clamp:fn but allows ?, !, and argument destructuring."
  (mvb (new-args alist) (parse-args args)
    (if (null alist)
        `(lambda ,new-args ,@body)
        `(lambda ,new-args
           (let ,(map #'cadr alist) (list ,@(map #'car alist))
             ,@body)))))
