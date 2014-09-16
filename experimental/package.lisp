(defpackage :clamp-experimental
  (:nicknames :experimental)
  (:use :clamp)
  (:shadow :coerce)
  (:export
   ;; From ssyntax.
   :w/ssyntax :defssyntax-test :defssyntax-macro :defssyntax-sym-mac :sdef
   
   ;; From destructuring.
   :ddef :dfn

   ;; From coerce.
   :coerce :defcoerce))

;;;; There must be someway to export all of the symbols in clamp
;;;; except for desired symbols.
