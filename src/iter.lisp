;;;; these are macros which allow for different kinds of iteration

(in-package :clamp)

(mac rec (withses &body body)
  "Same as loop in Anarki. Look for use cases"
  (let w (pair withses)
    `(funcall (rfn recur ,(map #'car w) ,@body) ,@(map #'cadr w))))

(mac repeat (n &body body)
  "Excutes the body n times"
  `(loop repeat ,n do (do ,@body)))

(mac up (var a b &body body)
  "Evaluates body iterating from a up to b exclusive"
  `(loop for ,var from ,a below ,b do (do ,@body)))

(mac upto (var a b &body body)
  "Evaluates body iterating from a up to b inclusive"
  `(loop for ,var from ,a upto ,b do (do ,@body)))

(mac downfrom (var a b &body body)
  "Evaluates body iterating from a down to b inclusive"
  `(loop for ,var downfrom ,a to ,b do (do ,@body)))

(mac down (var a b &body body)
  "Evaluates body iterating from a (exclusive) to b (inclusive)"
  `(downfrom ,var (1- ,a) ,b ,@body))

(mac while (test &body body)
  "Evaluates body while the test is true"
  `(loop while ,test do (do ,@body)))

(mac until (test &body body)
  "Evaluates body until the test is true"
  `(loop until ,test do (do ,@body)))

(mac each (var seq &body body)
 "Iterates across each element in seq. Currently works
  on both lists and arrays (maybe add hashtables in the future)"
 `(loop for ,var being the elements of ,seq do (do ,@body)))

(mac on (var seq &body body)
  "Same as each except simultaneously binds 'index' to the index of the element"
  `(loop for ,var being the elements of ,seq
	 for index from 0
	 do (do ,@body)))

(mac whilet (var test &body body)
  "Executes body until test returns nil. The value of test is bound
   to var on each iteration."
  `(loop for ,var = ,test
	 while ,var
	 do (do ,@body)))

(mac whiler (var expr endval &body body)
  "Executes body until the result of expr passes the testified 
   version of endval. The value of endval is bound to var on 
   each iteration."
  (w/uniq gtest
    `(loop with ,gtest = (testify ,endval)
	         for ,var = ,expr
      	   until (funcall ,gtest ,var)
      		 do (do ,@body))))
