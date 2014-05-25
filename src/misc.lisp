;;;; utilities which do not belong in any other file

(mac ado (&body body)
  "Evaluates each expression with it bound to the result of
   the previous one. Returns the value of the last expression"
  (lf (null body)
        nil
      (single body)
        (car body)
      `(let1 it ,(car body)
	(declare (ignorable it))
        (ado ,@(cdr body)))))

(mac accum (accfn &body body)
  "The result is all of the arguments passed into accfn"
  `(let1 gacc@ '()
     (flet1 ,accfn (garg) (push garg gacc@) ; uniq for garg seemingly not required but not taking any risks
       ,@body)
     (nrev gacc@)))

(def multiple (x y)
  "A predicate for testing if x is a multiple of y"
  (zerop (mod x y)))

(mac check (x test &optional alt)
  "If x passes the test, otherwise evaluate alt"
  `(let1 x@ ,x
     (lf (funcall ,test x@)
	 x@
	 ,alt)))

(defmacro zap (op place &rest args)
  "Assigns the value of applying op to the rest of the args
   to the second arg"
  (mvb (vars forms var set access)
       (get-setf-expansion place)
    `(withs (,@(mappend #'list vars forms) ,(car var) (funcall ,op ,access ,@args))
       ,set)))

(define-modify-macro or= (new)
  (lambda (var new) (lf var var new))
  "If the var is nil, it assigns the new value to it.
   Otherwise does nothing. This always evaluates new")

(mac in (x &rest choices)
  "Checks if the result of evaluating x is the result of
   one of the other arguments. Only evaluates arguments
   as necessary"
  `(let1 val@ ,x
     (or ,@(mapf (fn (c) `(is val@ ,c)) choices))))
