;;;; This is an experimental implementation of ssyntax.

(in-package :experimental)
(use-syntax :clamp)

;;;; By surrounding code with the w/ssyntax macro, ssyntax is
;;;; transformed into the corresponding code. To implement your own
;;;; ssyntax, use the defssyntax-test, defssyntax-sym-mac, and
;;;; defssyntax-macro.

;;;; The macro defssyntax-test is used to define a predicate which can
;;;; detect the different versions of ssyntax. The macros
;;;; defssyntax-sym-mac and defssyntax-macro and used to define
;;;; procedures which will expand the actual ssyntax. Use
;;;; defssyntax-sym-mac to define a procedure which will expand any
;;;; form that is not in the operator position. You must write a
;;;; procedure which can generate symbol-macrolet binding to be used
;;;; to expand the ssyntax. Ultimately this winds up being a list with
;;;; the first element being the symbol (the one that contains
;;;; ssyntax) and the desired transformation. The macro
;;;; defssyntax-macro is used to define a procedure which will expand
;;;; any ssyntax that is in the operator position. To use it, you need
;;;; to write a procedure which will generate a macro definition
;;;; (whose name is the symbol which contains the ssyntax) and whose
;;;; expansion will yield the desired transformation. If you are
;;;; confused just look at some of the examples below.

;;;; A procedure defined using any of the above methods must take in
;;;; two arguments. The first one will be the symbol that potentially
;;;; contains ssyntax. The second argument (provided for convenience)
;;;; is a string of the name of the symbol.

;;;; ISSUES
;;;; Using multiple ssyntax in the same symbol will expand in an
;;;; unpredictable way. Additionally the ssyntax for composition (+)
;;;; detects procedures such as 1+ and +. To get around this I used a
;;;; simple hack of testing that the length of the symbol is greater
;;;; than two for ssyntax for composition. Also there is no way to
;;;; specify ssyntax where any of the procedures used in the ssyntax
;;;; are stored in variables, for example:

;;;; (let f (fn (x) (+ x 10)) (list+f 20))

;;;; will look for the function f, not the variable.

(defmacro w/ssyntax (&body body)
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

(defun ssyntax (sym)
  "Does this contain ssyntax? If it does, the return value is the
   name of the kind of ssyntax."
  (find [call _ sym (symbol-name sym)]
        ssyntax-tests* :key #'cadr))

(defparameter errstr* "Wrong number of arguments in definition for ~
                        ~A in ~2&~A~2&~A supplied, ~A~@[ to ~A~] ~
                        expected.")

(defmacro defssyntax-test (&whole form name args &body body)
  "Defines a new test to detect ssyntax of the kind NAME."
  (check-len name form args 2 :str errstr*)
  `(push (list ',name (fn ,args ,@body))
         ssyntax-tests*))

(defmacro defssyntax-sym-mac (&whole form name args &body body)
  "Defines how to get the symbol-macrolet binding for the NAME kind
   of ssyntax."
  (check-len name form args 2 :str errstr*)
  `(= (gethash ',name ssyntax-sym-macs*)
      (fn ,args ,@body)))

(defmacro defssyntax-macro (&whole form name args &body body)
  "Defines how to get the macrolet binding for this kind of ssyntax."
  (check-len name form args 2 :str errstr*)
  `(= (gethash ',name ssyntax-macros*)
      (fn ,args ,@body)))

(defun ssyntax-sym-mac (sym)
  "Given a symbol that has ssyntax, returns the symbol-macrolet binding
   for it to be transformed into what it is supposed to be."
  (aand (ssyntax sym)
        (gethash (car it) ssyntax-sym-macs*)
        (call it sym (symbol-name sym))))

(defun ssyntax-macro (sym)
  "Given a symbol that has ssyntax, returns the macrolet definition for
   it to be a macro and expand correctly."
  (aand (ssyntax sym)
        (gethash (car it) ssyntax-macros*)
        (call it sym (symbol-name sym))))
