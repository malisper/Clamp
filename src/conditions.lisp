;;;; these are different kinds of conditionals

(defmacro lf (&body rest)
  "Cond but doesn't require parens for each clause"
  `(cond ,@(pair rest)))

(defmacro lflet (var expr &body branches)
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

(defmacro whenlet (var expr &body body)
  "Analog of lflet but for when"
  `(lflet ,var ,expr (progn ,@body)))

(defmacro alf (expr &body branches)
   "lflet but uses 'it' for var"
  `(lflet it ,expr ,@branches))

(defmacro awhen (expr &body body)
   "Analog of alf but for when"
  `(whenlet it ,expr ,@body))

(defmacro aand (&rest args)
  "Evaluates each argument one by one. Binds the result of the previous
   expression to 'it'. Otherwise the same as and"
  (lf (no args)
        t
      (no (cdr args))
        (car args)
      'else
        `(let1 it ,(car args) (and it (aand ,@(cdr args))))))
