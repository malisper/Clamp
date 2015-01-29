;;;; These are utilities for binding variables.

(in-package :clamp)

(mac with (parms &body body)
  "Equivalent to cl:let, but does not require parens around each
   individual binding. This also allows for destructuring."
  (let* ((pparms (pair parms))
	 (pats (map #'car  pparms))
	 (vals (map #'cadr pparms)))
    `(destructuring-bind ,pats (list ,@vals) ,@body)))

(mac let (var val &body body)
  "Equivalent to with, except binds only one variable."
  `(with (,var ,val) ,@body))

(mac ret (var val &body body)
  "Equivalent to clamp:let, but the result of a ret expression is the
   final value of VAR."
  `(let ,var ,val ,@body ,var))

;; I need to figure out a better name for this since the name makes it
;; seem like withs but it behaves like with.
(mac rets (parms &body body)
  "Same as with, but returns the values of all of the variables."
  `(with ,parms
     ,@body
     (values ,@(map #'car (pair parms)))))

(mac rets1 (parms &body body)
  "Same as rets but only returns the value of the first variable."
  `(with ,parms
     ,@body
     ,(car parms)))

(mac flet1 (name args fbody &body body)
  "Equivalent to flet, but only for one procedure definition."
  `(flet ((,name ,args ,fbody)) ,@body))

(mac withs (parms &body body)
  "Equivalent to let*, but allows destructuring."
  (if (no parms)
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms)
	 (withs ,(cddr parms) ,@body))))
