;;;; several tests for clamp
;;;; they are intended to be run while in the CLAMP package

(clunit:defsuite clamp ())

(clunit:defsuite base (clamp))

;;; figure out how to add test for reader macros

(clunit:deftest single (base)
  (clunit:assert-true  (single '(a)))
  (clunit:assert-false (single '()))
  (clunit:assert-false (single '(a b)))
  (clunit:assert-false (single '(a b c))))

(clunit:deftest pair (base)
  (clunit:assert-equal (pair '(a b c d)) '((a b) (c d)))
  (clunit:assert-equal (pair '(a b c)) '((a b) (c)))
  (clunit:assert-equal (pair '(1 2 3 4) #'+) '(3 7))
  (clunit:assert-equal (pair '(1 2 3) #'+) '(3 3)))

(clunit:deftest auto (base)
  (clunit:assert-true  (auto 'abc@))
  (clunit:assert-false (auto 'abc)))

(clunit:deftest if (base)
  (clunit:assert-expands (cond (a b) (c d)) (if a b c d))
  (clunit:assert-expands (cond (a b) (t c)) (if a b c)))
