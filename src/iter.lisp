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
