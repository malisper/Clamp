;;;; TODO
;;;; Add tests using some framework such as ASDF
;;;; write the rest of the functions from arc
;;;; rewrite arc functions to include keywords since arc doesn't have them
;;;; rewrite mac so it allows autouniq
;;;; figure out why code won't compile in sbcl (probably need to use eval-when)
;;;; start using some path system such as ASDF

(load "aliases.lisp")

;;; reader macro for literal fn notation with brackets
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\] (get-macro-character #\)))
  (set-macro-character #\[
    (fn (stream char)
	`(fn (_) (,@(read-delimited-list #\] stream t))))))

(def single (xs)
  "A predicate for testing whether a list has only one element"
  (and (consp xs) (no (cdr xs))))

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
  "Destructuring-bind (let) but doesn't require parens for each binding"
  (let* ((pparms (pair parms))
	 (pats (mapf #'car  pparms))
	 (vals (mapf #'cadr pparms)))
    `(destructuring-bind ,pats (list ,@vals) ,@body)))

(mac let1 (var val &body body)
  "With but with only one variable (no parens)"
  `(with (,var ,val) ,@body))

(mac ret (var val &body body)
  "Let1 but the result of the expression is the final value of the variable"
  `(let1 ,var ,val ,@body ,var))

(mac flet1 (name args fbody &body body)
  "Flet but with only one binding (no parens)"
  `(flet ((,name ,args ,fbody)) ,@body))

(mac withs (parms &body body)
  "With analog for let* but doesn't require parens around each binding"
  (lf (no parms)
      `(progn ,@body)
      `(let1 ,(car parms) ,(cadr parms)
	 (withs ,(cddr parms) ,@body))))

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
  (lf (functionp x) x [iso x _]))

(mac rec (withses &body body)
  "Same as loop in Anarki. Look for use cases"
  (let1 w (pair withses)
    `(funcall (rfn recur ,(mapf #'car w) ,@body) ,@(mapf #'cadr w))))

(def del (test xs)
  "Deletes all of the elements that pass test in the list xs. If test
   is a function all objects passing the test are removed. If test is
   anything else, all objects iso with it are removed
   NOTE: note the same as delete, as del is not a destructive function"
  (let1 f (testify test)
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
      `(let1 ,names (uniq (symbol-name ',names)) ,@body))) ; want to add better way to convert from symbol

(mac lflet (var expr &body branches)
  "Same as lf but if a predicate is true, it is bound to var"
  (lf branches
      (w/uniq gv
	`(let1 ,gv ,expr
	   (lf ,gv
	       (let1 ,var ,gv
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
  `(let1 it ,expr (lf it (progn ,@body)))) ; change name of progn

(mac aand (&rest args)
  "Evaluates each argument one by one. Binds the result of the previous
   expression to 'it'. Otherwise the same as and"
  (lf (no args)
        t
      (no (cdr args))
        (car args)
      'else
        `(let1 it ,(car args) (and it (aand ,@(cdr args))))))

(def memo (f)
  "Returns a memoized version of the function f"
  (let1 cache (table :test #'iso)
    (fn (&rest args)
      (mvb (value in) (gethash args cache)
	(lf in
	    value
	    (setf (gethash args cache) (apply f args)))))))

(mac defmemo (name args &body body)
  "Defines a memoized function"
  `(setf (symbol-function ',name)
	 (memo (fn ,args ,@body))))

(mac ado (&body body)
  "Evaluates each expression with it bound to the result of
   the previous one. Returns the value of the last expression"
  (lf (null body)
        nil
      (single body)
        (car body)
      `(let1 it ,(car body)
        (ado ,@(cdr body)))))

(mac accum (accfn &body body)
  "The result is all of the arguments passed into accfn"
  (w/uniq (gacc garg)
    `(let1 ,gacc '()
       (flet1 ,accfn (,garg) (push ,garg ,gacc) ; uniq for garg seemingly not required but not taking any risks
	 ,@body)
       (nrev ,gacc))))

(def curry (f &rest args1)
  "Returns a function with its left most arguments passed in and waiting for the rest"
  (fn (&rest args2) (apply f (append args1 args2))))

(def rcurry (f &rest args1)
  "Returns a function with its right most arguments passed in and waiting for the rest"
  (fn (&rest args2) (apply f (append args2 args1))))

(mac check (x test &optional alt)
  "If x passes the test, otherwise evaluate alt"
  (w/uniq gx
    `(let1 ,gx ,x
       (lf (funcall ,test ,gx)
	   ,gx
	   ,alt))))

(def multiple (x y)
  "A predicate for testing if x is a multiple of y"
  (zerop (mod x y)))

;;; iteration macros to avoid using loop directly
(mac up (var a b &body body)
  "Evaluates body iterating from a up to b inclusive"
  `(loop for ,var from ,a to ,b do (progn ,@body)))

(mac down (var a b &body body)
  "Evaluates body iterating from a down to b inclusive"
  `(loop for ,var from ,a downto ,b do (progn ,@body)))

(mac while (test &body body)
  "Evaluates body while the test is true"
  `(loop while ,test do (progn ,@body)))

(mac until (test &body body)
  "Evaluates body until the test is true"
  `(loop until ,test do (progn ,@body)))

(mac each (var seq &body body)
 "Iterates across each element in seq. Currently works
  on both lists and arrays (maybe add hashtables in the future)"
 `(loop for ,var being the elements of ,seq do (progn ,@body)))
