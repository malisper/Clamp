;;;; these are functions that need to be loaded before the macros in clamp.lisp

;;; reader macro for literal fn notation with brackets
(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[
  (fn (stream char)
      (declare (ignore char))
      `(fn (_) (,@(read-delimited-list #\] stream t)))))

(def single (xs)
  "A predicate for testing whether a list has only one element"
  (and (consp xs) (no (cdr xs))))

(def pair (xs &optional (f #'list))
    "Applies a function f to every two elements in xs"
    (cond ((no xs) '())
	  ((single xs) (list (list (car xs))))
	  ('else (cons (funcall f (car xs) (cadr xs))
		       (pair (cddr xs) f)))))

(def mappend (f xs)
    "Joins the results of mapping f over xs"
    (apply #'join (mapf f xs)))

(def const (x)
  "Evaluates to a function which always evaluates to x"
  (fn (&rest args) (declare (ignore args)) x))

(def alter (old new seq)
  "Substitues everything passes the testified version of old
   with new (which can be a function)"
  (with (test (testify old) next (if (typep new 'function)
				     new
				     (const new)))
    (rec (tree seq)
       (lf (atom tree)
	   (lf (funcall test tree)
	       (funcall next tree)
	       tree)
	   (cons (recur (car tree))
		 (recur (cdr tree)))))))

(def auto (x)
  "Checks if some expression should be auto-uniqd"
  (and x (symbolp x) (ado (symbol-name x)
			  (eql #\@ (elt it (1- (len it)))))))

