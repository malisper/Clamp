;;;; these are macros which allow for different kinds of iteration

(in-package "CLAMP")

(mac rec (withses &body body)
  "Same as loop in Anarki. Look for use cases"
  (let w (pair withses)
    `(funcall (rfn recur ,(map #'car w) ,@body) ,@(map #'cadr w))))

(mac repeat (n &body body)
  "Excutes the body n times"
  `(loop repeat ,n do (do ,@body)))

(mac up (var a b &body body)
  "Evaluates body iterating from a up to b inclusive"
  `(loop for ,var from ,a to ,b do (do ,@body)))

(mac down (var a b &body body)
  "Evaluates body iterating from a down to b inclusive"
  `(loop for ,var from ,a downto ,b do (do ,@body)))

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
