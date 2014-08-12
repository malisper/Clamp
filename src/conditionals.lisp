;;;; these are different kinds of conditionals

(in-package :clamp)

(mac iflet (var expr &body branches)
  "Same as if but if a predicate is true, it is bound to var"
  (if branches
      (w/uniq gv
	`(let ,gv ,expr
	   (if ,gv
	       (let ,var ,gv
		 (declare (ignorable ,@(flat var)))
		 ,(car branches))
	       ,(if (cdr branches)
		    `(iflet ,var ,@(cdr branches))))))
      expr))

(mac whenlet (var expr &body body)
  "Analog of iflet but for when"
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr &body branches)
   "iflet but uses 'it' for var"
  `(iflet it ,expr ,@branches))

(mac awhen (expr &body body)
   "Analog of aif but for when"
  `(whenlet it ,expr ,@body))

(mac aand (&rest args)
  "Evaluates each argument one by one. Binds the result of the previous
   expression to 'it'. Otherwise the same as and"
  (if (no args)
        t
      (no (cdr args))
        (car args)
      :else
        `(let it ,(car args)
	   (declare (ignorable it))
	   (and it (aand ,@(cdr args))))))

(mac aif2 (&rest clauses)
  "aif but for working with functions that have multiple return values
   ie gethash. See On Lisp for examples"
  (w/uniq (val win)
    (if (null clauses)
          nil
	(single clauses)
          (car clauses)
	:else
        (let (t1 c1 . rest) clauses
	  `(mvb (,val ,win) ,t1
	     (if (or ,val ,win)
		 (let it ,val (declare (ignorable it)) ,c1)
		 (aif2 ,@rest)))))))

(mac case (keyform &rest clauses)
  "Same as regular CL case except there are no parens around each pair."
  `(cl:case ,keyform ,@(group clauses)))

(mac ccase (keyform &rest clauses)
  "Same as regular CL ccase except there are no parens around each pair."
  `(cl:ccase ,keyform ,@(group clauses)))

(mac ecase (keyform &rest clauses)
  "Same as regular CL ecase except there are no parens around each pair."
  `(cl:ecase ,keyform ,@(group clauses)))
