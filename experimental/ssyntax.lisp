;;;; This is an experimental implementation of ssyntax.

(in-package :clamp)

;;;; By surrounding code with the w/ssyntax macro, ssyntax is
;;;; transformed into the corresponding code. To implement your own
;;;; ssyntax, use the defssyntax-test, defssyntax-sym-mac, and
;;;; defssyntax-macro.

;;;; The macro defssyntax-test allows you define a function which will
;;;; test for a specific kind ssyntax. The macro defssyntax-macro
;;;; allows for the defintion of a function which will calculate a
;;;; macrolet binding for the ssyntax. The macro will be expanded
;;;; whenever the ssyntax occurs in the function position of a
;;;; function call. The macro defssyntax-sym-mac allows for the
;;;; definition of how to calculate a symbol-macrolet binding for the
;;;; symbol which will be used whenever the symbol is not in the
;;;; calling position.

;;;; ISSUES
;;;; The ssyntax for composition (+) detects procedures such as 1+ and
;;;; +. To get around this I used a simple hack of testing that the
;;;; length is greater than two for ssyntax for composition. Also
;;;; there is no way to specify ssyntax where any of the procedures
;;;; used in the ssyntax are stored in variables, for example:

;;;; (let f (fn (x) (+ x 10)) (list+f 20))

;;;; will look for the function f, not the variable.

(mac w/ssyntax (&body body)
  "Allows BODY to use ssyntax."
  (let syms (keep #'ssyntax (redup (keep #'symbolp (flat body))))
    `(macrolet ,(trues #'ssyntax-macro syms)
       (symbol-macrolet ,(trues #'ssyntax-sym-mac syms)
         ,@body))))

(defparameter ssyntax-tests* '()
  "A list of fns used to test for ssyntax.")

(defparameter ssyntax-sym-macs* (table)
  "A table of fns used to transform ssyntax sym-macs into regular 
   syntax.")

(defparameter ssyntax-macros* (table)
  "A table of fns used to transform ssyntax macros into regular 
   syntax.")

(def ssyntax (sym)
  "Does this contain ssyntax? If it does, this returns the name of
   the kind of ssyntax."
  (find [funcall _ sym (symbol-name sym)]
        ssyntax-tests* :key #'cadr))

(mac defssyntax-test (kind arg &body body)
  "Defines a new test to detect ssyntax of the kind KIND."
  `(push (list ',kind (fn ,arg ,@body))
         ssyntax-tests*))

(mac defssyntax-sym-mac (kind arg &body body)
  "Defines how to get the symbol-macrolet binding for this kind of 
   ssyntax."
  `(= (gethash ',kind ssyntax-sym-macs*)
      (fn ,arg ,@body)))

(mac defssyntax-macro (name arg &body body)
  "Defines how to get the macrolet binding for this kind of ssyntax."
  `(= (gethash ',name ssyntax-macros*)
      (fn ,arg ,@body)))

(def ssyntax-sym-mac (sym)
  "Given a symbol that has ssyntax, returns the symbol-macrolet binding
   for it to be transformed into what it is supposed to be."
  (aand (ssyntax sym)
        (gethash (car it) ssyntax-sym-macs*)
        (funcall it sym (symbol-name sym))))

(def ssyntax-macro (sym)
  "Given a symbol that has ssyntax, returns the macrolet definition for
   it to be a macro and expand correctly."
  (aand (ssyntax sym)
        (gethash (car it) ssyntax-macros*)
        (funcall it sym (symbol-name sym))))

(defssyntax-test notf (sym name)
  (declare (ignore sym))
  (and (len> name 1)
       (char= #\~ (char name 0))))

(defssyntax-sym-mac notf (sym name)
  `(,sym (notf #',(intern (cut name 1)))))

(defssyntax-macro notf (sym name)
  (declare (ignore name))
  `(,sym (&body body)
     `(funcall ,',sym ,@body)))

(defssyntax-test compose (sym name)
  (declare (ignore sym))
  (and (pos #\+ name)
       (len> name 2))) ; This removes + and 1+ from being detected.

(defssyntax-sym-mac compose (sym name)
  (let pos (pos #\+ name)
    `(,sym (compose #',(intern (cut name 0 pos))
                    #',(intern (cut name (+ pos 1)))))))

(defssyntax-macro compose (sym name)
  (let pos (pos #\+ name)
    `(,sym (&body body)
       ;; Expand from (x+y ...) to (x (y ...)).
       `(,',(intern (cut name 0 pos))
            (,',(intern (cut name (+ pos 1)))
                ,@body)))))

(defssyntax-test andf (sym name)
  (declare (ignore sym))
  (find #\& name))

(defssyntax-sym-mac andf (sym name)
  (let pos (pos #\& name)
    `(,sym (andf #',(intern (cut name 0 pos))
                 #',(intern (cut name (+ pos 1)))))))

(defssyntax-macro andf (sym name)
  (declare (ignore name))
  `(,sym (&body body)
     `(funcall ,',sym ,@body)))

(defgeneric access (obj arg)
  (:documentation "Returns whatever is associated with ARG in OBJ."))

(defmethod access ((seq sequence) (n number))
  "Returns the Nth element of a sequence."
  (elt seq n))

(defmethod access ((tab hash-table) x)
  "Returns whatever is stored in TAB under X."
  (gethash x tab))

(defmethod access (object x)
  "Calls X on OBJECT."
  (funcall x object))

(defssyntax-test access (sym name)
  (declare (ignore sym))
  (find #\. name))

(defssyntax-sym-mac access (sym name)
  (let pos (pos #\. name)
    `(,sym (access ,(read-from-string (cut name 0 pos))
                   ,(read-from-string (cut name (+ pos 1)))))))

(defssyntax-test quote-access (sym name)
  (declare (ignore sym))
  (find #\! name))

(defssyntax-sym-mac quote-access (sym name)
  (let pos (pos #\! name)
    `(,sym (access ,(read-from-string (cut name 0 pos))
                   ',(read-from-string (cut name (+ pos 1)))))))
