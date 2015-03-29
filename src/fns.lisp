;;;; These are macros which allow for creation of procedures.

(in-package :clamp)

(mac rfn (name parms &body body)
  "Creates a recursive procedure which can refer to itself through 
   NAME."
  `(labels ((,name ,parms ,@body))
     #',name))

(mac afn (parms &body body)
  "Creates a recursive procedure which can refer to itself through 
   the symbol 'self'."
  `(rfn self ,parms ,@body))

(mac rec (withses &body body)
  "Bind the WITHSES and execute BODY. Using 'recur' allows a
   recursive 'jump' to the top of the body with the new bindings
   passed into recur. This is very similar to loop in clojure,
   but this allows multiple recursive calls."
  (let w (pair withses)
    `(call (rfn recur ,(map #'car w) ,@body) ,@(map #'cadr w))))
