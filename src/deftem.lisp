;;;; This defines deftem, a macro that makes defining a class similar
;;;; to defining a structure. If no initial value is given, slots
;;;; default to nil.

(in-package :clamp)

(mac deftem (name-and-options &rest slots)
  (withs (slot-names (map #'carif slots)
	  name (carif name-and-options)
	  options (if (listp name-and-options) (cdr name-and-options) '())
	  constructor-name (or2 (alref options :constructor) (symb 'make- name))
	  predicate-name (or2 (alref options :predicate) (symb name '-p))
	  conc-name (or2 (alref options :conc-name) (symb name '-)))
    `(do (defclass ,name ()
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
	      `(def ,constructor-name (&rest ,args &key ,@slot-names)
		 ,(tostring (prf "Create an object of type ~(~A~)." name))
		 (declare (ignore ,@slot-names))
		 (apply #'make-instance ',name ,args))))
	  
	  (defmethod print-object ((obj ,name) stream)
	    ,(tostring (prf "Print an object of type ~(~A~)." name))
	    (print-unreadable-object (obj stream :type t)
	      (with-slots ,slot-names obj
		(format stream "~{:~A ~S~^ ~}"
		        (list ,@(mappendeach n slot-names `(',n ,n)))))))
          ',name)))
