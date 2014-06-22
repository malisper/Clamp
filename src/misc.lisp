;;;; utilities which do not belong in any other file

(in-package "CLAMP")

(mac ado (&body body)
  "Evaluates each expression with it bound to the result of
   the previous one. Returns the value of the last expression"
  (if (null body)
        nil
      (single body)
        (car body)
      `(let it ,(car body)
	(declare (ignorable it))
        (ado ,@(cdr body)))))

(mac accum (accfn &body body)
  "The result is all of the arguments passed into accfn"
  (w/uniq gacc
    `(let ,gacc '()
       (flet1 ,accfn (arg) (push arg ,gacc)
	 ,@body)
       (nrev ,gacc))))

(def multiple (x y)
  "A predicate for testing if x is a multiple of y"
  (zerop (mod x y)))

(mac check (x test &optional alt)
  "If x passes the test, otherwise evaluate alt"
  (w/uniq (val))
    `(let ,val ,x
       (if (funcall ,test ,val)
	   ,val
	   ,alt)))

(mac zap (op place &rest args)
  "Assigns the value of applying op to the rest of the args
   to the second arg"
  (mvb (vars forms var set access)
       (get-setf-expansion place)
    `(withs (,@(mappend #'list vars forms) ,(car var) (funcall ,op ,access ,@args))
       ,set)))

(mac or= (place new)
  "If the var is nil, it assigns the new value to it.
   Otherwise does nothing"
  (mvb (vars forms var set access)
       (get-setf-expansion place)
    `(withs (,@(mappend #'list vars forms) ,(car var) (or ,access ,new))
       ,set)))

(mac in (x &rest choices)
  "Checks if the result of evaluating x is the result of
   one of the other arguments. Only evaluates arguments
   as necessary"
  (w/uniq val
    `(let ,val ,x
       (or ,@(map (fn (c) `(is ,val ,c)) choices)))))

(mac cart (f xs ys)
  "Applies the function f to a subset of the cartesian product of xs and 
   ys. The element from xs is bound to 'it' which can be used to change what
   ys is"
  (w/uniq y
    `(mapcan (fn (it) ; we can use mapcan because map creates new conses
		 (map (fn (,y)
			  (funcall ,f it ,y))
		      ,ys))
	     ,xs)))
