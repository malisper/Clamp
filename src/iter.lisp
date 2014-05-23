;;;; these are macros which allow for different kinds of iteration

(mac rec (withses &body body)
  "Same as loop in Anarki. Look for use cases"
  (let1 w (pair withses)
    `(funcall (rfn recur ,(mapf #'car w) ,@body) ,@(mapf #'cadr w))))

(mac repeat (n &body body)
  "Excutes the body n times"
  `(loop repeat ,n do (progn ,@body)))

(mac up (var a b &body body)
  "Evaluates body iterating from a up to b inclusive"
  `(loop for ,var from ,a to ,b do (progn ,@body)))

(mac down (var a b &body body)
  "Evaluates body iterating from a down to b inclusive"
  `(loop for ,var from ,a downto ,b do (progn ,@body)))

(mac while (test &body body)
  "Evaluates body while the test is true"
  `(loop while ,test do (progn ,@body)))

(mac until (test &body body)
  "Evaluates body until the test is true"
  `(loop until ,test do (progn ,@body)))

(mac each (var seq &body body)
 "Iterates across each element in seq. Currently works
  on both lists and arrays (maybe add hashtables in the future)"
 `(loop for ,var being the elements of ,seq do (progn ,@body)))
