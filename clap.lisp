;;;; TODO
;;;; Add tests
;;;; write the rest of the functions from arc
;;;; figure out why code won't compile

(load "aliases.lisp")

(def single (xs)
  "A predicate for testing whether a list has only one element"
  (and (isa xs 'cons) (no (cdr xs))))

(mac lf (&rest rest)
  "Cond but doesn't require parens for each clause"
  (cond ((no rest) nil)
        ((single rest) (car rest))
	('else `(if ,(car rest)
		    ,(cadr rest)
		    (lf ,@(cddr rest))))))

(def pair (xs &optional (f #'list))
  "Applies a function f to every two elements in xs"
  (lf (no xs)
        nil
      (no (cdr xs))
        (list (list (car xs)))
      'else
        (cons (funcall f (car xs) (cadr xs))
	      (pair (cddr xs) f))))

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
  "Creates a recursive function whcih can refer to itself through 'self'"
  `(labels ((self ,parms ,@body))
     #'self))

(def testify (x)
  "If passed a function, returns it. Otherwise returns a function which
   tests equality for the object passed"
  (lf (isa x 'function) x (fn (y) (iso y x))))

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
