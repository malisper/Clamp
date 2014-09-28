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

(deftest zap (misc)
  (assert-eql 100 (ret x 10 (zap [* _ _] x)))
  (assert-equal (range 1 10)
    (ret x (range 1 5)
      (zap #'append x (range 6 10)))))

(deftest or= (misc)
  (let x nil
    (assert-eql 5 (or= x 5))
    (assert-eql 5 (or= x 7)))
  ;; This is for the problem or2= is supposed to fix.
  (let tab (table)
    (assert-eql nil (or= (gethash 'a tab) nil))
    (assert-eql 5   (or= (gethash 'a tab) 5))))

(deftest or2= (misc)
  (let tab (table)
    (assert-eql 5   (or2= (gethash 'a tab) 5))
    (assert-eql 5   (or2= (gethash 'a tab) 7))
    (assert-eql nil (or2= (gethash 'b tab) nil))
    (assert-eql nil (or2= (gethash 'b tab) 5))))

(deftest in (misc)
  (assert-true  (in (+ 1 1) 1 2 3))
  (assert-false (in (+ 1 1) 1 3))
  (assert-true  (in (+ 1 1) (+ 1 0) (+ 1 1) (+ 2 1)))
  (assert-true  (in (+ 1 1) 1 2 (/ 1 0) 3)))

(deftest cart (misc)
  (assert-equal '((a a) (a b) (b a) (b b)) (cart #'list '(a b) '(a b)))
  (assert-equal '((a b) (b a)) (cart #'list '(a b) (rem it '(a b))))
  (assert-equal '(1 2 3 4) (cart #'+ '(1 3) '(0 1))))
