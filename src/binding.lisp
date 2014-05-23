;;;; these are macros which create new bindings

(defmacro with (parms &body body)
  "Destructuring-bind (let) but doesn't require parens for each binding"
  (let* ((pparms (pair parms))
	 (pats (mapf #'car  pparms))
	 (vals (mapf #'cadr pparms)))
    `(destructuring-bind ,pats (list ,@vals) ,@body)))

(defmacro let1 (var val &body body)
  "With but with only one variable (no parens)"
  `(with (,var ,val) ,@body))

(defmacro ret (var val &body body)
  "Let1 but the result of the expression is the final value of the variable"
  `(let1 ,var ,val ,@body ,var))

(defmacro flet1 (name args fbody &body body)
  "Flet but with only one binding (no parens)"
  `(flet ((,name ,args ,fbody)) ,@body))

(defmacro withs (parms &body body)
  "With analog for let* but doesn't require parens around each binding"
  (if (no parms)
      `(progn ,@body)
      `(let1 ,(car parms) ,(cadr parms)
	 (withs ,(cddr parms) ,@body))))
