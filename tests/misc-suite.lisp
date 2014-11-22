(in-package :clamp-tests)

(defsuite misc (clamp))

(deftest ado (misc)
  (assert-eql 338350 (ado (range 1 100)
                          (map [* _ _] it)
                          (reduce #'+ it)))
  (assert-eql 10 (ado 10)))

(deftest accum (misc)
  (assert-equal (range 1 10)
    (accum a
      (upto i 1 10
        (a i))))
  (assert-equal (map [* _ _] (range 1 10))
    (accum a
      (upto i 1 10
        (a (* i i))))))

(deftest summing (misc)
  (assert-eql 5
              (summing s
                (each x (range 1 10)
                  (s (even x))))))

(deftest multiple (misc)
  (assert-true  (multiple 10 5))
  (assert-false (multiple 15 2)))

(deftest check (misc)
  (assert-eql 5  (check 5 #'odd  10))
  (assert-eql 10 (check 5 #'even 10)))

(deftest acheck (misc)
  (assert-eql 20 (acheck 20 #'even (+ it 5)))
  (assert-eql 25 (acheck 20 #'odd  (+ it 5))))

(deftest in (misc)
  (assert-true  (in (+ 1 1) 1 2 3))
  (assert-false (in (+ 1 1) 1 3))
  (assert-true  (in (+ 1 1) (+ 1 0) (+ 1 1) (+ 2 1)))
  (assert-true  (in (+ 1 1) 1 2 (/ 1 0) 3)))

(deftest cart (misc)
  (assert-equal '((a a) (a b) (b a) (b b)) (cart #'list '(a b) '(a b)))
  (assert-equal '((a b) (b a)) (cart #'list '(a b) (rem it '(a b))))
  (assert-equal '(1 2 3 4) (cart #'+ '(1 3) '(0 1))))

(deftest point (misc)
  (assert-eql 10 (point val
                   (loop for i from 2 to 100 by 2
                         if (multiple i 5)
                           do (val i))))
  (assert-eql 10 (point val
                   (map (fif [multiple _ 5] #'val)
                        (range 2 100 2)))))

(deftest roundup (misc)
  (assert-eql 5 (roundup 4.6))
  (assert-eql 5 (roundup 5.4))
  (assert-eql 5 (roundup 4.5))
  (assert-eql 6 (roundup 5.5))
  (assert-eql -5 (roundup -4.6))
  (assert-eql -5 (roundup -5.4))
  (assert-eql -5 (roundup -4.5))
  (assert-eql -6 (roundup -5.5)))

(deftest nearest (misc)
  (assert-eql 5 (nearest 4.5 1))
  (assert-eql 5 (nearest 5.4 1))
  (assert-eql 6 (nearest 5.5 1))
  (assert-eql 6 (nearest 4.5 3))
  (assert-eql 3 (nearest 4.4 3))
  ;; I need to rewrite this test since floats are inexact when doing
  ;; math with them.
  ;; (assert-eql 3.14 (nearest 3.14159265 .01))
  )

(deftest before (misc)
  (let xs '(1 2 3 4 5)
    (assert-true  (before 1 2 xs))
    (assert-true  (before 4 5 xs))
    (assert-true  (before 5 6 xs))
    (assert-false (before 6 7 xs))
    (assert-false (before 2 1 xs))
    (assert-false (before 5 4 xs))
    (assert-false (before 6 5 xs))
    (assert-true  (before #'odd #'even xs))
    (assert-false (before #'even #'odd xs))
    (assert-true  (before [multiple _ 3] [multiple _ 4] xs))
    (assert-false (before [multiple _ 4] [multiple _ 3] xs))
    (assert-true  (before [multiple _ 3] 4 xs))
    (assert-false (before 4 [multiple _ 3] xs))))
