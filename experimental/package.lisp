(defpackage :clamp-experimental
  (:nicknames :experimental)
  (:use :clamp)
  (:shadow :coerce :def :fn :mac :defmemo)
  (:export
   ;; From ssyntax.
   :w/ssyntax :defssyntax-test :defssyntax-macro :defssyntax-sym-mac
   
   ;; From destructuring.
   :fn

   ;; From coerce.
   :coerce :defcoerce

   ;; From def.
   :def :defmemo :mac))

;;;; There must be someway to export all of the symbols in clamp
;;;; except for desired symbols.
