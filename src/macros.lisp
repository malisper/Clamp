;;;; macros for writing macros

(in-package :clamp)

(mac w/uniq (names &body body)
  "Binds each element in names (or names if it is just a symbol), with
   a unique symbol"
  (if (consp names)
      `(with ,(mappend (fn (n) `(,n (uniq (symbol-name ',n))))
                       names)
             ,@body)
      `(let ,names (uniq (symbol-name ',names)) ,@body)))
