;;;; This defines deftem, a macro that makes defining a class similar
;;;; to defining a structure. If no initial value is given, slots
;;;; default to nil.

(in-package :clamp)

(defclass template () ()
  (:documentation "The template base class."))

(defgeneric get-slots (obj)
  (:documentation "Returns all of the slots of the template OBJ.")
  (:method-combination append :most-specific-last))

(defmethod print-object ((tem template) stream)
  "Print the template by printing all of the slots and their values."
  (pprint-logical-block (stream (get-slots tem))
    (print-unreadable-object (tem stream :type t)
      (pprint-indent :current 0 stream)
      (pprint-exit-if-list-exhausted)
      (format stream ":~A ~S" (pprint-pop) (pprint-pop))
      (loop (pprint-exit-if-list-exhausted)
         (format stream " ~_:~A ~S" (pprint-pop) (pprint-pop)))))
  tem)

(mac deftem (name-and-options &rest slots)
  "Define a class with a syntax similar to that of defstruct."
  (withs (slot-names (map #'carif slots)
	  name (carif name-and-options)
	  options (if (listp name-and-options) (cdr name-and-options) '())
	  constructor-name (or2 (alref options :constructor) (symb 'make- name))
	  predicate-name (or2 (alref options :predicate) (symb name '-p))
	  conc-name (or2 (alref options :conc-name) (symb name '-))
          printer-name (alref options :print-object)
          direct-superclasses (alref options :include t))
    `(do (defclass ,name (,@direct-superclasses template)
	   ,(mapeach s slots
	      (let (slot-name &optional initform) (mklist s)
		`(,slot-name :accessor ,(if conc-name (symb conc-name slot-name) slot-name)
			     :initarg  ,(intern (mkstr slot-name) :keyword)
			     :initform ,initform))))

	 ,(when predicate-name
	    `(def ,predicate-name (object)
	       ,(tostring (prf "Is OBJECT of type ~(~A~)?" name))
	       (typep object ',name)))

         ,(when constructor-name
	    (w/uniq args
	      `(def ,constructor-name (&rest ,args &key ,@slot-names &allow-other-keys)
		 ,(tostring (prf "Create an object of type ~(~A~)." name))
		 (declare (ignore ,@slot-names))
		 (apply #'make-instance ',name ,args))))

         (defmethod get-slots append ((obj ,name))
           ,(tostring (prf "Returns a flat list of the slots that belong to a ~(~A~) and their values." name))
           (with-slots ,slot-names obj
             (list ,@(mappendeach n slot-names `(',n ,n)))))

         ,(when printer-name
           `(defmethod print-object ((obj ,name) stream)
              ,(tostring (prf "Print an object of type ~(~A~)." name))
              (call #',printer-name obj stream)))

         ',name)))
