;;;; definitions for defalias which allows redefinition of macros/fns

(in-package :clamp)

(defun macrop (x)
  "Is x a macro?"
  (and (symbolp x) (macro-function x)))

(defun make-macro (new old)
  "Generates the code for making the macros new and old equivalent."
  (cl:let ((rest (gensym "REST")))
    `(progn
       (defmacro ,new (&rest ,rest) `(,',old ,@,rest))
       (setf (documentation ',new 'function)
             (documentation ',old 'function)
             (macro-function ',new)
             (macro-function ',old)))))

(defun fnp (x)
  "Is x a function?"
  (and (symbolp x) (symbol-function x)))

(defun make-fn (new old)
  "Generates the code for making new and old the same function."
  ;; compiler macros may be risky if they rely on using the same
  ;; symbol they are named with so they are not included
  (cl:let ((args (gensym "ARGS")))
    `(progn
       (defun ,new (&rest ,args) (apply #',old ,args))
       (setf (documentation ',new 'function)
             (documentation ',old 'function)
             (symbol-function ',new)
             (symbol-function ',old)))))

(defun make-special-macro (new old)
  "Generates the code to create a macro 'new' which expands into a use 
   of the special form 'old'."
  (cl:let ((rest (gensym "REST")))
    `(defmacro ,new (&rest ,rest)
       `(,',old ,@,rest))))

(defmacro defalias (new old)
  "Makes a use of new the equivalent to a use of old."
  (cond ((special-operator-p old)
         (make-special-macro new old))
        ((macrop old)
         (make-macro new old))
        ((fnp old)
         (make-fn new old))
        (:else
         (error "Don't know what to do for object ~A of type ~A"
                old (type-of old)))))

