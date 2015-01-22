;;;; This defines deftem, a macro that makes defining a class similar
;;;; to defining a structure. If no initial value is given, slots
;;;; default to nil.

(in-package :clamp)

(defgeneric print-slots (obj stream)
  (:documentation "Print all of the slots of this class."))

(mac deftem (name-and-options &rest slots)
  (withs (slot-names (map #'carif slots)
	  name (carif name-and-options)
	  options (if (listp name-and-options) (cdr name-and-options) '())
	  constructor-name (or2 (alref options :constructor) (symb 'make- name))
	  predicate-name (or2 (alref options :predicate) (symb name '-p))
	  conc-name (or2 (alref options :conc-name) (symb name '-))
          printer-name (alref options :print-object)
          direct-superclasses (cdr (assoc :include options)))
    `(do (defclass ,name ,direct-superclasses
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

         ;; If there are any superclasses make this an after method.
         ;; Otherwise make it a primary method.
         (defmethod print-slots ,@(if direct-superclasses (list :after) '())
                                ((obj ,name) stream)
           (with-slots ,slot-names obj
             ;; Print a space to begin with if there are superclasses
             ;; who will print their slots before this.
             (format stream "~:[~; ~]~{:~A ~S~^ ~}"
                     ',direct-superclasses
                     (list ,@(mappendeach n slot-names `(',n ,n))))))

         ;; Use the default print-object for this class unless it
         ;; either specifies it's own or does not have any
         ;; superclasses to inherit one from.
         ,(when (or printer-name (no direct-superclasses))
            `(defmethod print-object ((obj ,name) stream)
               ,(tostring (prf "Print an object of type ~(~A~)." name))
               ,(if printer-name
                    `(call #',printer-name obj stream)
                    `(print-unreadable-object (obj stream :type t)
                       (print-slots obj stream)))))

         ',name)))
