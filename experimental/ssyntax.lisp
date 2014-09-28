;;;; This is an experimental implementation of ssyntax.

(in-package :clamp-experimental)

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

(defun ssyntax (sym)
  "Does this contain ssyntax? If it does, this returns the name of
   the kind of ssyntax."
  (find [call _ sym (symbol-name sym)]
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

(defssyntax-test notf (sym name)
  (declare (ignore sym))
  (and (len> name 1)
       (char= #\~ (char name 0))))

(defssyntax-sym-mac notf (sym name)
  `(,sym (notf #',(intern (cut name 1)))))

(defssyntax-macro notf (sym name)
  `(,sym (&body body)
     `(not (,',(intern (cut name 1)) ,@body))))

(defssyntax-test compose (sym name)
  (declare (ignore sym))
  (and (pos #\+ name)
       (len> name 2))) ; This removes + and 1+ from being detected.

(defssyntax-sym-mac compose (sym name)
  (ado (tokens name #\+)
       (map #'intern it)
       (map (fn (f) `#',f) it)
       `(,sym (compose ,@it))))

(defssyntax-macro compose (sym name)
  (ado (tokens name #\+)
       (map #'intern it)
       `(,sym (&body body)
          ;; (f+g+h ...) will expand into (f (g (h ...))) therefore
          ;; we need to work from the back and create a new list
          ;; containing the fn and the previous expression.
          (reduce #'list ',(butlast it)
                  :from-end t
                  :initial-value `(,',(last1 it) ,@body)))))

(defssyntax-test andf (sym name)
  (declare (ignore sym))
  (find #\& name))

(defssyntax-sym-mac andf (sym name)
  (ado (tokens name #\&)
       (map #'intern it)
       (map (fn (f) `#',f) it)
       `(,sym (andf ,@it))))

(defssyntax-macro andf (sym name)
  (declare (ignore name))
  `(,sym (&body body)
     `(call ,',sym ,@body)))

(defgeneric access (obj arg)
  (:documentation "Returns whatever is associated with ARG in OBJ."))

(defgeneric (setf access) (val obj arg)
  (:documentation "Sets ARG to be associated with VAL in OBJ."))

(defmethod access ((seq sequence) (n number))
  "Returns the Nth element of a sequence."
  (elt seq n))

(defmethod (setf access) (val (seq sequence) (n number))
  "Sets the Nth element of SEQ to VAL."
  (= (elt seq n) val))

(defmethod access ((tab hash-table) x)
  "Returns whatever is stored in TAB under X."
  (gethash x tab))

(defmethod (setf access) (val (tab hash-table) x)
  "Sets VAL to be stored under X in TAB."
  (= (gethash x tab) val))

(defmethod access (obj x)
  "Calls X on OBJECT."
  (call x obj))

;; A setter for the default case would have to lookup the setter
;; for the given argument.

(defun access-ssyntax (c)
  (in c #\. #\!))

(defssyntax-test access (sym name)
  (declare (ignore sym))
  (find #'access-ssyntax name))

(defssyntax-sym-mac access (sym name)
  (withs (ssyntaxes (keep #'access-ssyntax name)
          (obj . accessors) (map #'read-from-string
                                 (tokens name #'access-ssyntax))
          calls (map (fn (ss accessor)
                       (if (is ss #\.) accessor `',accessor))
                     ssyntaxes
                     accessors))
    `(,sym ,(reduce (fn (exp accessor)
                      `(access ,exp ,accessor))
                    calls
                    :initial-value obj))))
