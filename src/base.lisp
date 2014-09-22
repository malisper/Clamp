;;;; These are some basic utilities which need to be loaded first.

(in-package :clamp)

(def map (f seq &rest seqs)
  "Maps F over the sequences. The returned sequence will be the same
   type as SEQ."
  (apply #'cl:map (type-of seq) f seq seqs))

;;; This cannot be defined as an alias because then it would expand
;;; into #'(fn ..) which is an error.
(mac fn (args &body body)
  "Equivalent to lambda except this cannot be used as the name of
   of function (ie ((fn ..) ..))."
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
        (:else (cons (funcall f (car xs) (cadr xs))
                     (pair (cddr xs) f)))))

(mac if (&rest clauses)
  "Equivalent to cond, but does not require parens parens around each
   individual clause."
  ;; SBCL deduces that the value of the cond can be nil when there
  ;; only a return value for the else clause. Because of this the
  ;; else clause needs to be explicit about that last value.
  (cl:if (even (len clauses))
    `(cond ,@(pair clauses))
    `(cond ,@(pair (butlast clauses)) ,(cons t (last clauses))))) 
