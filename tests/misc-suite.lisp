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
