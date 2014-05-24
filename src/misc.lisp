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

(define-modify-macro zap (f &rest args)
  "Calls f on the variable with the addition arguments
   and sets the variable to that result"
  (lambda (var f &rest args) (apply f var args))) ; need to use lambda because of lambda in functional position

(define-modify-macro or= (new)
  "If the var is nil, it assigns the new value to it.
   Otherwise does nothing. This always evaluates new"
  (lambda (var new) (lf var var new)))
