;;;; These are utilities for conditional branching.

(in-package :clamp)

(mac iflet (var &body branches)
  "Same as clamp:if but if a predicate is true, the value of predicate 
   is bound to VAR in the corresponding branch."
  (if (no branches)
        nil
      (single branches)
        (car branches)
      :else
        (w/uniq val
          ;; Since the list has at least two elements, the first one
          ;; is the test and the second is the expr to evaluate if
          ;; the test returns true.
          (let (test expr . rest) branches
            ;; A uniq needs to be used in case VAR is dynamically scoped.
            `(let ,val ,test
               (if ,val
                   (let ,var ,val
                     (declare (ignorable ,@(flat var)))
                     ,expr)
                   (iflet ,var ,@rest)))))))

(mac whenlet (var expr &body body)
  "If EXPR returns non-nil, bind that value to VAR and execute body."
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr &body branches)
   "Equivalent to iflet but automatically binds EXPR to 'it'."
  `(iflet it ,expr ,@branches))

(mac awhen (expr &body body)
   "Equivalent to whenlet but automatically binds the value of EXPR
    to 'it'."
  `(whenlet it ,expr ,@body))

(mac aand (&rest args)
  "Equivalent to and, but binds the value of the previous expr to 
   'it'."
  (if (no args)
        t
      (no (cdr args))
        (car args)
      :else
        `(let it ,(car args)
	   (declare (ignorable it))
	   (and it (aand ,@(cdr args))))))

(mac iflet2 (var &rest branches)
  "Equivalent to iflet, but will also execute the corresponding branch
   if the predicate has a second return value which is non-nil. This
   is useful for accessing hashtables."
    (if (no branches)
        nil
      (single branches)
        (car branches)
      :else
        (w/uniq (val win)
          ;; Since the list has at least two elements, the first one
          ;; is the test and the second is the expr to evaluate if
          ;; the test returns true.
          (let (test expr . rest) branches
            ;; A uniq needs to be used in case VAR is dynamically scoped.
            `(mvb (,val ,win) ,test
               (if (or ,val ,win)
                   (let ,var ,val
                     (declare (ignorable ,@(flat var)))
                     ,expr)
                   (iflet2 ,var ,@rest)))))))

(mac aif2 (&rest clauses)
  "Equivalent to aif, but will also execute the corresponding branch
   if the predicate has a second return value which is non-nil. This
   is useful for accessing hashtables."
  `(iflet2 it ,@clauses))

(mac aand2 (&rest exps)
  "Equivalent to and, but binds the value of the previous expr to 
   'it' and this considers a non-nil second return value to be true."
  (if (no exps)
        t
      (no (cdr exps))
        (car exps)
      :else
        (w/uniq (val win)
          `(mvb (,val ,win) ,(car exps)
             (and (or ,val ,win)
                  (let it ,val
                    (declare (ignorable it))
                    (aand2 ,@(cdr exps))))))))

(mac case (keyform &rest clauses)
  "Equivalent to cl:case except there are no parens around each 
   clause."
  `(cl:case ,keyform ,@(group clauses)))

(mac ccase (keyform &rest clauses)
  "Equivalent to cl:ccase except there are no parens around each
   clause.
   WARNING: do not use :else as the default case, that means to
   test for the symbol :else. Instead you have to use t."
  `(cl:ccase ,keyform ,@(group clauses)))

(mac ecase (keyform &rest clauses)
  "Equivalent to cl:ecase except there are no parens around each
   clause.
   WARNING: do not use :else as the default case, that means to
   test for the symbol :else. Instead you have to use t."
  `(cl:ecase ,keyform ,@(group clauses)))

(mac caselet (var val &rest clauses)
  "The result of VAL is assigned to VAR and then it is compared
   against each case clause.
   WARNING: do not use :else as the default case, that means to
   test for the symbol :else. Instead you have to use t."
  `(let ,var ,val
     (case ,var
       ,@clauses)))

(mac typecase (keyform &rest clauses)
  "Equivalent to cl:typecase but does not require parens around each
   clause.
   WARNING: do not use :else as the default case, that means to
   test for the symbol :else. Instead you have to use t."
  `(cl:typecase ,keyform ,@(group clauses)))
