(in-package :experimental)
(use-syntax :clamp)

;; The macro w/lisp1 works by defining every symbol within the body to
;; expand into itself surrounded by a call to function. The symbol
;; will not be expanded again since function is a special form. Due to
;; the fact that symbol-macrolet is lexically scoped, it is still
;; possible to define variables within the w/lisp1 form, although it
;; is impossible to refer to symbols outside of it. You cannot use any
;; symbol that is defined as a global variable within the w/lisp1,
;; because it is an error to define a symbol-macrolet with the same
;; name as a global variable. It is currently not possible to call
;; variables as procedures.

(defmacro w/lisp1 (&rest body)
  "Evaluate BODY as if in a lisp-1."
  `(symbol-macrolet ,(mapeach sym (redup (keep #'symbolp (flat body)))
                      `(,sym #',sym))
     ,@body))
