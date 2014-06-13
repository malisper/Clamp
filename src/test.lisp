;;;; several tests for clamp
;;;; they are intended to be run while in the CLAMP package

(clunit:defsuite clamp ())

(clunit:defsuite base (clamp))
(clunit:defsuite binding (clamp))

;;; figure out how to add tests for reader macros

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

(clunit:deftest with (binding)
  (clunit:assert-eql (with (a 1 b 2 c 3) (+ a b c)) 6)
  (clunit:assert-eql (with ((x y) (list 1 2) z 3) (+ x y z)) 6)
  (clunit:assert-equal (with (a 1 b 2) (with (a b b a) (list a b)))
                       '(2 1)))

(clunit:deftest let (binding)
  (clunit:assert-expands (with (a b) c) (let a b c))
  (clunit:assert-eql (let x 3 (+ x 5)) 8)
  (clunit:assert-eql (let (x . y) (cons 1 2) (+ x y)) 3))

(clunit:deftest ret (binding)
  (clunit:assert-eql (ret x 5 (incf x 10)) 15)
  (clunit:assert-equal (ret x '() (push 'b x) (push 'a x)) '(a b)))

(clunit:deftest flet1 (binding)
  (clunit:assert-expands (flet ((a (x y z) b))) (flet1 a (x y z) b)))

(clunit:deftest withs (binding)
  (clunit:assert-eql (withs (x 5 y (+ x 3)) (+ y 4)) 12))
