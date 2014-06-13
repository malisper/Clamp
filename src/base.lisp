;;;; these are some basic functions which need to be loaded first

(in-package "CLAMP")

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
	  ((single xs) (list (funcall f (car xs))))
	  ('else (cons (funcall f (car xs) (cadr xs))
		       (pair (cddr xs) f)))))

(def auto (x)
  "Checks if some expression should be auto-uniqd"
  (and x (symbolp x) (eql #\@ (elt (symbol-name x) (1- (len (symbol-name x)))))))

(defmacro if (&rest clauses)
  "Cond but doesn't require parens for each clause"
  ;; the if is needed to prevent warnings from occuring
  (cl:if (even (len clauses))
	 `(cond ,@(pair clauses))
	 `(cond ,@(pair (butlast clauses)) ,(cons t (last clauses))))) 

