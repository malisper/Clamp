;;;; these are different kinds of conditionals

(mac lflet (var expr &body branches)
  "Same as lf but if a predicate is true, it is bound to var"
  (lf branches
      (w/uniq gv
	`(let1 ,gv ,expr
	   (lf ,gv
	       (let1 ,var ,gv
		 (declare (ignorable ,var))
		 ,(car branches))
	       ,(lf (cdr branches)
		    `(lflet ,var ,@(cdr branches))))))
      expr))

(mac whenlet (var expr &body body)
  "Analog of lflet but for when"
  `(lflet ,var ,expr (progn ,@body)))

(mac alf (expr &body branches)
   "lflet but uses 'it' for var"
  `(lflet it ,expr ,@branches))

(mac awhen (expr &body body)
   "Analog of alf but for when"
  `(whenlet it ,expr ,@body))

(mac aand (&rest args)
  "Evaluates each argument one by one. Binds the result of the previous
   expression to 'it'. Otherwise the same as and"
  (lf (no args)
        t
      (no (cdr args))
        (car args)
      'else
        `(let1 it ,(car args) (and it (aand ,@(cdr args))))))

(mac alf2 (&rest clauses)
  "alf but for working with functions that have multiple return values
   ie gethash. See On Lisp for examples"
  (lf (null clauses)
        nil
      (single clauses)
        (car clauses)
      'else
        (let1 (t1 c1 . rest) clauses
	  `(mvb (@val @win) ,t1
	     (lf (or @val @win)
		 (let1 it @val ,c1)
		 (alf2 ,@rest))))))
