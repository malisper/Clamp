(in-package :clamp-tests)

(defsuite io (clamp))

(deftest allchars (io)
  (w/instring in "hello goodbye"
   (assert-equalp "hello goodbye" (allchars in)))
  (w/instring in (tostring (prf "hello~%goodbye~%"))
    (assert-equalp (tostring (prf "hello~%goodbye~%")) (allchars in))))
