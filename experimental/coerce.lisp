;;;; This is an experimental implementation of customizable coercion.

(in-package :experimental)

(defgeneric coerce (obj to)
  (:documentation "Coerces OBJ to type TO."))

(defmethod coerce (obj to)
  "Default to cl:coerce."
  (cl:coerce obj to))

(defmacro defcoerce (from to args &body body)
  "Defines a coercer from type FROM to type TO. ARGS is a list of
   arguments needed for the coercion, so far only one argument is
   supported."
  `(defmethod coerce ((,(car args) ,from) (,(uniq) (eql ',to)))
     ,@body))

(defcoerce hash-table list (tab)
  "Coerce from hash-table to list."
  (tablist tab))

(defcoerce list hash-table (xs)
  "Coerce from list to hash-table."
  (listtab xs))
