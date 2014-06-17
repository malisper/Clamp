;;;; several tests for clamp
;;;; TODO: create new package which imports from clunit and clamp

(in-package "CLAMP")

(clunit:defsuite clamp ())

(clunit:defsuite base (clamp))
(clunit:defsuite binding (clamp))
(clunit:defsuite hof (clamp))

;;; base

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

;;; binding

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

;;; hof

(clunit:deftest testify (hof)
  (clunit:assert-true  (funcall (testify 5) 5))
  (clunit:assert-false (funcall (testify 5) 4))
  (clunit:assert-true  (funcall (testify #'even) 4))
  (clunit:assert-false (funcall (testify #'even) 5)))

(clunit:deftest rem (hof)
  (clunit:assert-equal (rem 5 '()) '())
  (clunit:assert-equal (rem #'even '()) '())
  (clunit:assert-equal (rem 5 '(1 5 2 8 2 5)) '(1 2 8 2))
  (clunit:assert-equal (rem #'even '(2 5 29 5 28)) '(5 29 5))
  (clunit:assert-equal (rem #'even '(2 12 16 4)) '())
  (clunit:assert-equal (rem #'even '(13 5 7)) '(13 5 7)))

(clunit:deftest keep (hof)
  (clunit:assert-equal (keep 7 '()) '())
  (clunit:assert-equal (keep #'even '()) '())
  (clunit:assert-equal (keep #'even '(1 2 8 2 3 4)) '(2 8 2 4))
  (clunit:assert-equal (keep #'even (5 7 3)) '())
  (clunit:assert-equal (keep #'even '(2 12 72 6)) '(2 12 72 6)))

(clunit:deftest mem (hof)
  (clunit:assert-false (mem 7 '()))
  (clunit:assert-false (mem #'even '()))
  (clunit:assert-false (mem 3 '(1 29 32 5)))
  (clunit:assert-equal (mem 5 '(1 6 3 5 3 2)) '(5 3 2))
  (clunit:assert-equal (mem #'even '(1 9 2 3)) '(2 3)))

(clunit:deftest find (hof)
  (clunit:assert-false (find 5 '()))
  (clunit:assert-false (find #'even '()))
  (clunit:assert-false (find 5 '(2 9 1 2 7 3)))
  (clunit:assert-eql (find 5 '(1 3 5 2 9 3)) 5)
  (clunit:assert-eql (find #'even '(1 3 5 2 9 3 4 6 7)) 2))

(clunit:deftest count (hof)
  (clunit:assert-eql (count 2 '()) 0)
  (clunit:assert-eql (count #'even '()) 0)
  (clunit:assert-eql (count #'even '(1 3 71 21)) 0)
  (clunit:assert-eql (count 5 '(1 5 3 2 5 7 5)) 3)
  (clunit:assert-eql (count #'even '(1 6 3 2 2 4)) 4))

(clunit:deftest pos (hof)
  (clunit:assert-false (pos 2 '()))
  (clunit:assert-false (pos #'even '()))
  (clunit:assert-false (pos #'even '(123 45 3 7)))
  (clunit:assert-eql (pos 5 '(1 3 5 3 2 5)) 2)
  (clunit:assert-eql (pos #'even '(1 7 3 2 5 7 4 2)) 3))

(clunit:deftest mappend (hof)
  (clunit:assert-equal (mappend #'identity '()) '())
  (clunit:assert-equal (mappend #'list '(1 2 3) '(4 5 6)) '(1 4 2 5 3 6)))

(clunit:deftest const (hof)
  (clunit:assert-eql (funcall (const 2)) 2)
  (clunit:assert-eql (funcall (const 5) 7) 5)
  (clunit:assert-eql (funcall (const 6) 3 2 1) 6))

(clunit:deftest subst (hof)
  (clunit:assert-equal (subst 3 5 '()) '())
  (clunit:assert-equal (subst 3 5 '((7 3 . 2) (5 . 3) . 7))
                       '((7 5 . 2) (5 . 5) . 7))
  (clunit:assert-equal (subst #'even 5 '((3 2 . 4) . 3)) '((3 5 . 5) . 3))
  (clunit:assert-equal (subst 2 #'1- '((3 . 2) (2 . 7))) '((3 . 1) (1 . 7)))
  (clunit:assert-equal (subst #'even #'1+ '((4 . 3) 2 . 5))
                       '((5 . 3) 3 . 5)))
