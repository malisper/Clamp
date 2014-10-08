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
