;;;; these are different kinds of conditionals

(mac lf (&body rest)
  "Cond but doesn't require parens for each clause"
  `(cond ,@(pair rest)))

(mac check (x test &optional alt)
  "If x passes the test, otherwise evaluate alt"
  (w/uniq gx
    `(let1 ,gx ,x
       (lf (funcall ,test ,gx)
	   ,gx
	   ,alt))))

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
