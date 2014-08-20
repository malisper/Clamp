;;;; Utilities which do not belong in any other file.

(in-package :clamp)

(mac ado (&body body)
  "Evaluates each expression with the symbol 'it' bound to the result
   of the previous one. Returns the value of the last expression."
  (if (null body)
        nil
      (single body)
        (car body)
      :else
        `(let it ,(car body)
           (declare (ignorable it))
           (ado ,@(cdr body)))))

(mac accum (accfn &body body)
  "Binds ACCFN to a function which will accumulate values. The result
   of accum is all of the elements passed to ACCFN in the same
   order."
  (w/uniq gacc
    `(let ,gacc '()
       (flet1 ,accfn (arg) (push arg ,gacc)
         ,@body)
       (nrev ,gacc))))

(def multiple (x y)
  "Is X a multiple of Y?"
  (zerop (mod x y)))

(mac check (x test &optional alt)
  "If X passes TEST (not testified) return it, otherwise evaluate 
   ALT."
  (w/uniq val
    `(let ,val ,x
       (if (funcall ,test ,val)
           ,val
           ,alt))))

(mac zap (op place &rest args)
  "Assigns the result of calling OP on the rest of the arguments 
   (including PLACE) to PLACE. For example (zap #'+ x n) is
   equivalent to (incf x n)."
  (mvb (vars forms var set access)
       (get-setf-expansion place)
    `(withs (,@(mappend #'list vars forms)
             ,(car var) (funcall ,op ,access ,@args))
       ,set)))

(mac or= (place new)
  "If PLACE is nil, assign the result of evaluating NEW there.
   Otherwise returns whatever value was already in PLACE and does not
   evaluate NEW."
  (mvb (vars forms var set access)
       (get-setf-expansion place)
    `(withs (,@(mappend #'list vars forms) ,(car var) (or ,access ,new))
       ,set)))

(mac or2= (place new)
  "Equivalent to or= but will not carry through with the assignment 
   if accessing PLACE has a second return value which is non-nil."
  (mvb (vars forms var set access)
       (get-setf-expansion place)
    (w/uniq (val win)
      `(withs (,@(mappend #'list vars forms))
         (mvb (,val ,win) ,access
           (let ,(car var) (if (or ,val ,win) ,val ,new)
             ,set))))))

(mac in (x &rest choices)
  "Returns t if X is one of the results of evaluating every CHOICE
   (lazily)."
  (w/uniq val
    `(let ,val ,x
       (or ,@(map (fn (c) `(is ,val ,c)) choices)))))

(mac cart (f xs ys)
  "Applies F to a variation of the cartesian product of XS and YS.
   While going through XS, every element is bound to 'it' which
   can be used to modify YS."
  (w/uniq y
    `(mapcan (fn (it) ; It is okay to use mapcan since map conses.
                 (map (fn (,y)
                          (funcall ,f it ,y))
                      ,ys))
             ,xs)))
