(in-package :clamp)

(define-package-syntax :clamp
  (:merge :standard)
  (:macro-char #\] (get-macro-character #\)))
  (:macro-char #\[ (lambda (stream char)
                     (declare (ignore char))
                     `(lambda (_)
                        (declare (ignorable _))
                        (,@(read-delimited-list #\] stream t))))))
