;;;; these are different kinds of conditionals

(in-package "CLAMP")

(mac iflet (var expr &body branches)
  "Same as lf but if a predicate is true, it is bound to var"
  (if branches
      (w/uniq gv
	`(let ,gv ,expr
	   (if ,gv
	       (let ,var ,gv
		 (declare (ignorable ,var))
		 ,(car branches))
	       ,(if (cdr branches)
		    `(iflet ,var ,@(cdr branches))))))
      expr))

(mac whenlet (var expr &body body)
  "Analog of lflet but for when"
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr &body branches)
   "lflet but uses 'it' for var"
  `(iflet it ,expr ,@branches))

(mac awhen (expr &body body)
   "Analog of alf but for when"
  `(whenlet it ,expr ,@body))

(mac aand (&rest args)
  "Evaluates each argument one by one. Binds the result of the previous
   expression to 'it'. Otherwise the same as and"
  (if (no args)
        t
      (no (cdr args))
        (car args)
      'else
        `(let it ,(car args) (and it (aand ,@(cdr args))))))

(mac aif2 (&rest clauses)
  "alf but for working with functions that have multiple return values
   ie gethash. See On Lisp for examples"
  (if (null clauses)
        nil
      (single clauses)
        (car clauses)
      'else
        (let (t1 c1 . rest) clauses
	  `(mvb (val@ win@) ,t1
	     (if (or val@ win@)
		 (let it val@ ,c1)
		 (aif2 ,@rest))))))

(mac case (keyform &rest clauses)
  `(cl:case ,keyform ,@(pair clauses)))
