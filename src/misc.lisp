;;;; utilities which do not belong in any other file

(def range (a b)
  "Generates the range of numbers from a to b"
  (lf (> a b)
      '()
      (cons a (range (1+ a) b))))

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
  (w/uniq (gacc garg)
    `(let1 ,gacc '()
       (flet1 ,accfn (,garg) (push ,garg ,gacc) ; uniq for garg seemingly not required but not taking any risks
	 ,@body)
       (nrev ,gacc))))

(def multiple (x y)
  "A predicate for testing if x is a multiple of y"
  (zerop (mod x y)))

(defmacro check (x test &optional alt)
  "If x passes the test, otherwise evaluate alt"
  (w/uniq gx
    `(let1 ,gx ,x
       (lf (funcall ,test ,gx)
	   ,gx
	   ,alt))))
