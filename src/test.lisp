;;;; several tests for clamp
;;;; TODO: figure out how to avoid duplication with packages

(defpackage "CLAMP-TESTS"
  (:use "CLAMP" "CLUNIT")
  (:export "CLAMP"))

(in-package :clamp-tests)

(defsuite clamp ())

(defsuite base (clamp))
(defsuite binding (clamp))
(defsuite hof (clamp))

;;; base

;;; figure out how to add tests for reader macros

(deftest single (base)
  (assert-true  (single '(a)))
  (assert-false (single '()))
  (assert-false (single '(a b)))
  (assert-false (single '(a b c))))

(deftest pair (base)
  (assert-equal (pair '(a b c d)) '((a b) (c d)))
  (assert-equal (pair '(a b c)) '((a b) (c)))
  (assert-equal (pair '(1 2 3 4) #'+) '(3 7))
  (assert-equal (pair '(1 2 3) #'+) '(3 3)))

(deftest if (base)
  (assert-expands (cond (a b) (c d)) (if a b c d))
  (assert-expands (cond (a b) (t c)) (if a b c)))

;;; binding

(deftest with (binding)
  (assert-eql (with (a 1 b 2 c 3) (+ a b c)) 6)
  (assert-eql (with ((x y) (list 1 2) z 3) (+ x y z)) 6)
  (assert-equal (with (a 1 b 2) (with (a b b a) (list a b)))
      '(2 1)))

(deftest let (binding)
  (assert-expands (with (a b) c) (let a b c))
  (assert-eql (let x 3 (+ x 5)) 8)
  (assert-eql (let (x . y) (cons 1 2) (+ x y)) 3))

(deftest ret (binding)
  (assert-eql (ret x 5 (incf x 10)) 15)
  (assert-equal (ret x '() (push 'b x) (push 'a x)) '(a b)))

(deftest flet1 (binding)
  (assert-expands (flet ((a (x y z) b))) (flet1 a (x y z) b)))

(deftest withs (binding)
  (assert-eql (withs (x 5 y (+ x 3)) (+ y 4)) 12))

;;; hof

(deftest testify (hof)
  (assert-true  (funcall (testify 5) 5))
  (assert-false (funcall (testify 5) 4))
  (assert-true  (funcall (testify #'even) 4))
  (assert-false (funcall (testify #'even) 5)))

(deftest rem (hof)
  (assert-equal (rem 5 '()) '())
  (assert-equal (rem #'even '()) '())
  (assert-equal (rem 5 '(1 5 2 8 2 5)) '(1 2 8 2))
  (assert-equal (rem #'even '(2 5 29 5 28)) '(5 29 5))
  (assert-equal (rem #'even '(2 12 16 4)) '())
  (assert-equal (rem #'even '(13 5 7)) '(13 5 7)))

(deftest keep (hof)
  (assert-equal (keep 7 '()) '())
  (assert-equal (keep #'even '()) '())
  (assert-equal (keep #'even '(1 2 8 2 3 4)) '(2 8 2 4))
  (assert-equal (keep #'even '(5 7 3)) '())
  (assert-equal (keep #'even '(2 12 72 6)) '(2 12 72 6)))

(deftest mem (hof)
  (assert-false (mem 7 '()))
  (assert-false (mem #'even '()))
  (assert-false (mem 3 '(1 29 32 5)))
  (assert-equal (mem 5 '(1 6 3 5 3 2)) '(5 3 2))
  (assert-equal (mem #'even '(1 9 2 3)) '(2 3)))

(deftest find (hof)
  (assert-false (find 5 '()))
  (assert-false (find #'even '()))
  (assert-false (find 5 '(2 9 1 2 7 3)))
  (assert-eql (find 5 '(1 3 5 2 9 3)) 5)
  (assert-eql (find #'even '(1 3 5 2 9 3 4 6 7)) 2))

(deftest count (hof)
  (assert-eql (count 2 '()) 0)
  (assert-eql (count #'even '()) 0)
  (assert-eql (count #'even '(1 3 71 21)) 0)
  (assert-eql (count 5 '(1 5 3 2 5 7 5)) 3)
  (assert-eql (count #'even '(1 6 3 2 2 4)) 4))

(deftest pos (hof)
  (assert-false (pos 2 '()))
  (assert-false (pos #'even '()))
  (assert-false (pos #'even '(123 45 3 7)))
  (assert-eql (pos 5 '(1 3 5 3 2 5)) 2)
  (assert-eql (pos #'even '(1 7 3 2 5 7 4 2)) 3))

(deftest mappend (hof)
  (assert-equal (mappend #'identity '()) '())
  (assert-equal (mappend #'list '(1 2 3) '(4 5 6)) '(1 4 2 5 3 6)))

(deftest subst (hof)
  (assert-equal (subst 3 5 '()) '())
  (assert-equal (subst 3 5 '((7 3 . 2) (5 . 3) . 7))
                       '((7 5 . 2) (5 . 5) . 7))
  (assert-equal (subst #'even 5 '((3 2 . 4) . 3)) '((3 5 . 5) . 3))
  (assert-equal (subst 2 #'1- '((3 . 2) (2 . 7))) '((3 . 1) (1 . 7)))
  (assert-equal (subst #'even #'1+ '((4 . 3) 2 . 5))
                       '((5 . 3) 3 . 5)))
