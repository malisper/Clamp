(defmacro defalias (new old)
  "Makes new the same as old"
  (cond ((macrop old)
	   (make-macro new old))
	((fnp old)
	   (make-fn new old))
	('else
	  (error "Don't know what to do for object ~A of type ~A" old (type-of old)))))

(defun macrop (x)
  "A predicate for testing whether x should be replaced with a macro"
  (and (symbolp x)
       (or (special-operator-p x)
	   (macro-function x))))

(defun make-macro (new old)
  "Generates the code for making new and old the same macro"
  (let ((rest (gensym "rest")))
    `(progn (setf (documentation ',new 'function) (documentation ',old 'function))
	    (defmacro ,new (&rest ,rest) ; need this for weird edge cases such as lambda
	      (cons ',old ,rest)))))

(defun fnp (x)
  "A predicate for testing whether x is a function"
  (and (symbolp x) (symbol-function x)))

(defun make-fn (new old)
  "Generates the code for making new and old the same function"
  `(setf (documentation ',new 'function) (documentation ',old 'function)
	 (symbol-function ',new) (symbol-function ',old)))

