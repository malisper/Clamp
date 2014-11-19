;;;; Utilities which do not belong in any other file.

(in-package :clamp)

(mac ado (&body body)
  "Evaluates each expression with the symbol 'it' bound to the result
   of the previous one. Returns the value of the last expression."
  ;; The special-form let* cannot be used here because it makes it
  ;; impossible to declare each 'it' ignorable. So if one expression
  ;; does not use 'it', a warning would be given.
  (if (null body)
        nil
      (single body)
        (car body)
      :else
        `(let it ,(car body)
           (declare (ignorable it))
           (ado ,@(cdr body)))))

(mac accum (accfn &body body)
  "Binds ACCFN to a procedure which will accumulate values. The result
   of accum is all of the elements passed to ACCFN in the same
   order."
  (w/uniq (ghead gtail)
    `(withs (,ghead (list nil) ,gtail ,ghead)
       (flet1 ,accfn (arg) (= ,gtail (= (cdr ,gtail) (list arg)))
         ,@body
         (cdr ,ghead)))))

(mac summing (sumfn &body body)
  "Binds SUMFN to a procudure which will counts every time it is
   passed a non-nil value. The result of summing is the number of
   times that procedure is called."
  (w/uniq gacc
    `(ret ,gacc 0
       (flet1 ,sumfn (arg) (when arg (++ ,gacc))
         ,@body))))

(def multiple (x y)
  "Is X a multiple of Y?"
  (zerop (mod x y)))

(mac check (x test &optional alt)
  "If X passes TEST (not testified) return it, otherwise evaluate 
   ALT."
  (w/uniq val
    `(let ,val ,x
       (if (call ,test ,val)
           ,val
           ,alt))))

(mac acheck (x test &optional alt)
 "If X passes TEST (not testified) return it, otherwise bind 'it' to
  the result of X and evaluate ALT."
 `(let it ,x
    (if (call ,test it)
        it
        ,alt)))

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
                          (call ,f it ,y))
                      ,ys))
             ,xs)))

(def rand-elt (seq)
  "Returns a random element from SEQ."
  (elt seq (rand (len seq))))

(mac rand-choice (&rest exprs)
  "Randomly evaluates one of the expressions in EXPRS."
  `(case (rand ,(len exprs))
     ,@(mappend #'list
                (range 0 (- (len exprs) 1))
                exprs)))

(mac point (name &body body)
  "Defines a procedure which when called on a value, the value of 
   this expression will immediately become that value. The procedure
   will only return up the stack, it is not the same as a 
   continuation."
  (w/uniq here
    `(block ,here
       (flet1 ,name (arg) (return-from ,here arg)
         ,@body))))

(mac defs (&body args)
  "Defines multiple procedures all in the same form."
  `(do ,@(mapeach proc (group args :by 3)
           `(def ,@proc))))

(def roundup (n)
  "Rounds the argument to the nearest number. Rounds halves away from
   zero."
  (mvb (base rem) (trunc n)
    (if (>= (abs rem) 1/2)
        (if (positive n)
            (inc base)
            (dec base))
        base)))

(def nearest (n quantum)
  "Rounds N to the closest multiple of QUANTUM. Halves are rounded 
   way from zero."
  (* (roundup (/ n quantum)) quantum))
