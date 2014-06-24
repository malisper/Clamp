;;;; several tests for clamp
;;;; TODO: figure out how to avoid duplication with packages

(defpackage "CLAMP-TESTS"
  (:use "CLAMP" "CLUNIT")
  (:export "CLAMP"))

(in-package "CLAMP-TESTS")

(defsuite clamp ())

(defsuite base (clamp))
(defsuite binding (clamp))
(defsuite hof (clamp))
(defsuite conditionals (clamp))

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

(deftest auto (base)
  (assert-true  (clamp::auto 'abc@))
  (assert-false (clamp::auto 'abc)))

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
  (assert-eql (ret x 5 (incf x 10) nil) 15)
  (assert-equal (ret x '() (push 'b x) (push 'a x) nil) '(a b)))

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

;;; conditionals

(deftest iflet (conditionals)
  (assert-eql (iflet x 5 (+ x 10)) 15)
  (assert-eql (iflet x (find #'even '(1 6 3 7)) (* x 2)) 12)
  (assert-eql (iflet x (find #'even '(1 3 7)) (+ 1 1) (+ 5 5)) 10)
  (assert-equal (iflet (x . y) (cons 5 10) (list x y)) '(5 10))
  (assert-equal (iflet (x . y) nil 10 (list 5 10) (cons x y))
                '(5 10))
  (assert-false (iflet nil nil nil nil)))

(deftest whenlet (conditionals)
  (assert-false (whenlet x nil 5))
  (assert-eql (whenlet (x . y) (cons 5 10) (+ x y)) 15)
  (assert-eql (whenlet x (+ 5 10) (+ 15 20) (+ 30 40)) 70))

(deftest aif (conditionals)
  (assert-false (aif nil t))
  (assert-eql (aif nil t 5) 5)
  (assert-eql (aif 5 it) 5)
  (assert-eql (aif 10 (+ it 5)) 15)
  (assert-eql (aif nil (+ it 5) 10 (+ it 20)) 30)
  (assert-eql (aif nil (+ it 5) nil (+ it 20) 15) 15))

(deftest awhen (conditionals)
  (assert-false (awhen nil t))
  (assert-eql (awhen (find #'even '(7 5 4 3)) (+ it 20)) 24)
  (assert-false (awhen (find #'even '(7 5 3)) (+ it 20)))
  (assert-eql (awhen (find #'even '(7 5 4 3)) (+ 5 10) (+ 15 20)) 35))

(deftest aand (conditionals)
  (assert-false (aand nil))
  (assert-false (aand t nil))
  (assert-false (aand t nil t))
  (let tab (obj a (obj a 1 b 2) b (obj a 1 b 2))
    (assert-eql (aand (gethash 'a tab) (gethash 'b it)) 2)
    (assert-false (aand (gethash 'c tab) (gethash 'b it)))))

(deftest aif2 (conditionals)
  (assert-false (aif2 nil (+ 5 5)))
  (assert-eql (aif2 (find #'even '(15 2 7 8)) (+ it 5)) 7)
  (let tab (obj a nil b 5)
    (assert-eql (aif2 (gethash 'b tab) (+ it 10)) 15)
    (assert-true (aif2 (gethash 'a tab) (not it)))
    (assert-false (aif2 (gethash 'c tab) (not it)))))

(deftest case (conditionals)
  (assert-false (case 'c a 1 b 2))
  (assert-eql (case 'a a 1 b 2) 1)
  (assert-eql (case 'b (a b) 1 c 2) 1)
  (assert-eql (case 'c a 1 b 2 t 3) 3)
  (assert-false (case 'c a 1 b 2 (t) 3))
  (assert-eql (case t a 1 b 2 (t) 3 t 4) 3))
