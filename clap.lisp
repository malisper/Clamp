;;;; TODO
;;;; Add tests
;;;; write the rest of the functions from arc
;;;; figure out why code won't compile

(load "./aliases.lisp")

(def single (xs)
  "A predicate for testing whether a list has only one element"
  (and (consp x) (no (cdr xs))))

(def pair (xs &optional (f #'list))
  "Applies a function f to every two elements in xs"
  (cond ((no xs) '())
	((single xs) (list (list (car xs))))
	('else (cons (funcall f (car xs) (cadr xs))
		     (pair (cddr xs) f)))))

(mac lf (&body rest)
  "Cond but doesn't require parens for each clause"
  `(cond ,@(pair rest)))

(mac with (parms &body body)
  "Let but doesn't require parens for each binding"
  `(let ,(pair parms) ,@body))

(mac lets (var val &body body)
  "Let but with only one variable (no parens)"
  `(let ((,var ,val)) ,@body))

(mac ret (var val &body body)
  "Lets but the result of the expression is the final value of the variable"
  `(lets ,var ,val ,@body ,var))

(mac flets (name args fbody &body body)
  "Flet but with only one binding (no parens)"
  `(flet ((,name ,args ,fbody)) ,@body))

(mac withs (parms &body body)
  "Same as let* but doesn't require parens around each binding"
  `(let* ,(pair parms) ,@body))

(mac rfn (name parms &body body)
  "Creates a recursive function which can refer to itself through name"
  `(labels ((,name ,parms ,@body))
     #',name))

(mac afn (parms &body body)
  "Creates a recursive function which can refer to itself through 'self'"
  `(labels ((self ,parms ,@body))
     #'self))

(def testify (x)
  "If passed a function, returns it. Otherwise returns a function which
   tests equality for the object passed"
  (lf (functionp x) x (fn (y) (iso y x))))

(mac rec (withses &body body)
  "Same as loop in Anarki. Look for use cases"
  (lets w (pair withses)
    `(funcall (rfn recur ,(mapf #'car w) ,@body) ,@(mapf #'cadr w))))

(def del (test xs)
  "Deletes all of the elements that pass test in the list xs. If test
   is a function all objects passing the test are removed. If test is
   anything else, all objects iso with it are removed"
  (lets f (testify test)
    (rec (xs xs)
      (lf (no xs)
	    '()
	  (funcall f (car xs))
	    (recur (cdr xs))
	  'else
	    (cons (car xs) (recur (cdr xs)))))))

(def keep (test xs)
  "Same as del but keeps the elements that pass the test"
  (del (complement (testify test)) xs))

(def range (a b)
  "Generates the range of numbers from a to b"
  (lf (> a b)
      '()
      (cons a (range (1+ a) b))))

(def mappend (f xs)
  "Joins the results of mapping f over xs"
  (apply #'join (mapf f xs)))

(mac w/uniq (names &body body)
  "Binds each element in names (or names if it is just a symbol), with
   a unique symbol"
  (lf (consp names)
      `(with ,(mappend (fn (n) `(,n (uniq (symbol-name ',n))))
		       names)
	 ,@body)
      `(lets ,names (uniq (symbol-name ',names)) ,@body))) ; want to add better way to convert from symbol

(mac lflet (var expr &body branches)
  "Same as lf but if a predicate is true, it is bound to var"
  (lf branches
      (w/uniq gv
	`(lets ,gv ,expr
	   (lf ,gv
	       (lets ,var ,gv
		 ,(car branches))
	       ,(lf (cdr branches)
		    `(lflet ,var ,@(cdr branches))))))
      expr))

(mac whenlet (var expr &body body)
  "Analog of lflet but for when"
  `(lflet ,var ,expr (progn ,@body))) ; change name of progn

(mac alf (expr &body branches)
   "lflet but uses 'it' for var"
  `(lflet it ,expr ,@branches))

(mac awhen (expr &body body)
   "Analog of alf but for when"
  `(lets it ,expr (lf it (progn ,@body)))) ; change name of progn

(mac aand (&rest args)
  "Evaluates each argument one by one. Binds the result of the previous
   expression to 'it'"
  (lf (no args)
        t
      (no (cdr args))
        (car args)
      'else
        `(lets it ,(car args) (and it (aand ,@(cdr args))))))
