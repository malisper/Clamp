;;;; These are macros which allow for different kinds of iteration.

(in-package :clamp)

(mac repeat (n &body body)
  "Excutes BODY N times."
  `(loop repeat ,n do (do ,@body)))

(mac up (var a b &body body)
  "Evaluates BODY iterating from A up to B exclusive."
  `(loop for ,var from ,a below ,b do (do ,@body)))

(mac upto (var a b &body body)
  "Evaluates BODY iterating from A up to B inclusive."
  `(loop for ,var from ,a upto ,b do (do ,@body)))

;; The name downfrom is a better name then downto because it includes
;; the higher number (the from) as opposed to down which does not.
(mac downfrom (var a b &body body)
  "Evaluates BODY iterating from A down to B inclusive."
  `(loop for ,var downfrom ,a to ,b do (do ,@body)))

(mac down (var a b &body body)
  "Evaluates BODY iterating from A (exclusive) to B (inclusive)."
  `(downfrom ,var (- ,a 1) ,b ,@body))

(mac while (test &body body)
  "Repeatedly evaluates BODY while TEST returns true."
  `(loop while ,test do (do ,@body)))

(mac until (test &body body)
  "Repeatedly evaluates BODY until TEST returns true."
  `(loop until ,test do (do ,@body)))

(mac each (var seq &body body)
 "Evaluates BODY while iterating across SEQ binding each element to
  VAR."
 `(loop for ,var in (coerce ,seq 'list) do (do ,@body)))

(mac on (var seq &body body)
  "Equivalent to each but binds the symbol 'index' to the position of
   the current element in SEQ."
  `(loop for ,var in (coerce ,seq 'list)
	 for index from 0
	 do (do ,@body)))

(mac whilet (var test &body body)
  "Executes BODY until TEST returns nil. The value of TEST is bound
   to VAR on each iteration."
  `(loop for ,var = ,test
	 while ,var
	 do (do ,@body)))

(mac whiler (var expr endval &body body)
  "Executes BODY until the result of expr passes the testified 
   version of ENDVAL. The value of EXPR is bound to VAR on each
   iteration."
  (w/uniq gtest
    `(loop with ,gtest = (testify ,endval)
	   for ,var = ,expr
      	   until (call ,gtest ,var)
           do (do ,@body))))

(mac forlen (var seq &body body)
  "Executes BODY, iterating from 0 to (len seq) (exclusive)."
  `(up ,var 0 (len ,seq)
     ,@body))

;;;; The following is a version of iterate that works within
;;;; Clamp. Iterate works only with symbols that are defined within
;;;; the iter package. That is meant to allow for shadowing of the
;;;; symbols in iterate, but that leads to a problem with clamp for
;;;; unintentional shadowing. This version works by searching for all
;;;; symbols with the same name as a symbol in the iterate package and
;;;; swaps them. This is really hacky, but it is the only thing I
;;;; could think of.

;;;; Originally I was using macrolet and symbol-macrolet, but that was
;;;; actually worse than this is. With sublis, only a symbol in the
;;;; lexical space can be replaced. With with the macrolets, symbols
;;;; could be replaced, even if they were put there by a macro. This
;;;; would lead to problems with let, since it expands into with,
;;;; which shares a name with a symbol in iterate.

(def in-iterate (sym)
  "Is there a symbol with the same name as SYM in the iterate
   package? If so, return it."
  (unless (is (symbol-package sym) (find-package :iter))
    (mvb (iter-sym accessibility) (find-symbol (symbol-name sym) :iter)
      (and (is accessibility :external) iter-sym))))

(mac iter (&rest forms)
  "Clamp's version of the iterate macro."
  (let syms (redup (keep (andf #'symbolp #'in-iterate) (flat forms)))
    `(iter:iter ,@(sublis (map [cons _ (in-iterate _)] syms)
                          forms))))
