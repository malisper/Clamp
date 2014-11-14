;;;; These are some basic utilities which need to be loaded first.

(in-package :clamp)

(def map (f seq &rest seqs)
  "Maps F over the sequences. The returned sequence will always be of
   type list."
  (apply #'cl:map 'list f seq seqs))

;;; This cannot be defined as an alias because then it would expand
;;; into #'(fn ..) which is an error.
(mac fn (args &body body)
  "Equivalent to lambda except this cannot be used as the name of
   of a procedure (ie ((fn ..) ..))."
  `(lambda ,args ,@body))

;;; This is a reader macro for literal fn notation with brackets.
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  (fn (stream char)
      (declare (ignore char))
      `(lambda (_)
         (declare (ignorable _))
         (,@(read-delimited-list #\] stream t)))))

(def single (xs)
  "Does this list have one and only one element?"
  (and (consp xs) (no (cdr xs))))

(def pair (xs &optional (f #'list))
  "Applies F to every two elements of xs and collects the results."
  (cond ((no xs) '())
        ((single xs) (list (funcall f (car xs))))
        (:else (cons (call f (car xs) (cadr xs))
                     (pair (cddr xs) f)))))

(mac if (&rest clauses)
  "Equivalent to cond, but does not require parens parens around each
   individual clause."
  ;; For some reason, SBCL deduces that the value of the cond can be 
  ;; nil when there is only a return value for the else clause of a 
  ;; cond (cond .. (x)). Because of this the else clause is explicit 
  ;; about the last value so that SBCL doesn't freak out.
  (cl:if (even (len clauses))
    `(cond ,@(pair clauses))
    `(cond ,@(pair (butlast clauses)) ,(cons t (lastcons clauses))))) 
