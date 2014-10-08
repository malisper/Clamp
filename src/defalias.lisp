;;;; Definitions for defalias which allows redefinition of
;;;; macros, procedures, and special forms.

(in-package :clamp)

(defun macrop (x)
  "Is this a macro?"
  (and (symbolp x) (macro-function x)))

(defun make-macro (new old &optional doc)
  "Generates the code for making the macros NEW and OLD equivalent."
  (cl:let ((rest (gensym "REST")))
    `(progn
       (defmacro ,new (&rest ,rest) `(,',old ,@,rest))
       (setf (macro-function ',new)
             (macro-function ',old)
             (documentation ',new 'function)
             ,(or doc `(documentation ',old 'function))))))

(defun fnp (x)
  "Is this a procedure?"
  (and (symbolp x) (symbol-function x)))

(defun make-fn (new old &optional doc)
  "Generates the code for making NEW and OLD the same procedure."
  (cl:let ((args (gensym "ARGS")))
    `(progn
       (defun ,new (&rest ,args) (apply #',old ,args))
       (setf (symbol-function ',new)
             (symbol-function ',old)
             (documentation ',new 'function)
             ,(or doc `(documentation ',old 'function)))
       ;; Define a compiler macro which expands into old.
       (define-compiler-macro ,new (&rest args)
         `(,',old ,@args)))))

(defun make-special-macro (new old)
  "Generates the code to create a macro NEW which expands into a use 
   of the special form OLD."
  (cl:let ((rest (gensym "REST")))
    `(defmacro ,new (&rest ,rest)
       `(,',old ,@,rest))))

(defmacro defalias (new old &optional doc)
  "Makes a use of NEW the equivalent to a use of OLD. Works on
   procedures, macros, and (most) special forms."
  (cond ((special-operator-p old)
         (make-special-macro new old))
        ((macrop old)
         (make-macro new old doc))
        ((fnp old)
         (make-fn new old doc))
        (:else
         (error "Don't know what to do for object ~A of type ~A"
                old (type-of old)))))

