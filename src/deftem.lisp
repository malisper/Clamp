;;;; This defines deftem, a macro that makes defining a class similar
;;;; to defining a structure. If no initial value is given, slots
;;;; default to nil.

(in-package :clamp)

(mac deftem (name &rest slots)
  (let names (map #'carif slots)
    `(do (defclass ,name ()
	   ,(mapeach s slots
	      (let (slot-name &optional initform) (mklist s)
		`(,slot-name :accessor ,(symb name '- slot-name)
			     :initarg  ,(intern (mkstr slot-name) :keyword)
			     :initform ,initform))))
	   
	 (def ,(symb name '-p) (object)
	   ,(tostring (prf "Is OBJECT of type ~(~A~)?" name))
	   (typep object ',name))
	  
         ,(w/uniq args
	    `(def ,(symb 'make- name) (&rest ,args &key ,@names)
	       ,(tostring (prf "Create an object of type ~(~A~)." name))
	       (declare (ignore ,@names))
	       (apply #'make-instance ',name ,args)))
	  
	  (defmethod print-object ((obj ,name) stream)
	    ,(tostring (prf "Print an object of type ~(~A~)." name))
	    (print-unreadable-object (obj stream :type t)
	      (with-slots ,names obj
		(format stream "~{:~A ~A~^ ~}"
		        (list ,@(mappendeach n names `(',n ,n))))))))))
