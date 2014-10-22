(in-package :clamp-tests)

(defsuite conditionals (clamp))

(deftest iflet (conditionals)
  (assert-eql 15 (iflet x 5 (+ x 10)))
  (assert-eql 10 (iflet x nil 5 10))
  (assert-eql 12 (iflet x (find #'even '(1 6 3 7)) (* x 2)))
  (assert-eql 10 (iflet x (find #'even '(1 3 7)) (+ 1 1) (+ 5 5)))
  (assert-equal '(5 10) (iflet (x . y) (cons 5 10) (list x y)))
  (assert-equal '(5 10)
                (iflet (x . y) nil 10 (list 5 10) (cons x y)))
  (assert-false (iflet nil nil nil nil))
  (assert-false (iflet x (gethash 'a (obj a nil)) (list x))))

(deftest whenlet (conditionals)
  (assert-false (whenlet x nil 5))
  (assert-eql 15 (whenlet (x . y) (cons 5 10) (+ x y)))
  (assert-eql 70 (whenlet x (+ 5 10) (+ 15 20) (+ 30 40))))

(deftest aif (conditionals)
  (assert-false (aif nil t))
  (assert-eql 5 (aif nil t 5))
  (assert-eql 5 (aif 5 it))
  (assert-eql 15 (aif 10 (+ it 5)))
  (assert-eql 30 (aif nil (+ it 5) 10 (+ it 20)))
  (assert-eql 15 (aif nil (+ it 5) nil (+ it 20) 15)))

(deftest awhen (conditionals)
  (assert-false (awhen nil t))
  (assert-eql 24 (awhen (find #'even '(7 5 4 3)) (+ it 20)))
  (assert-false (awhen (find #'even '(7 5 3)) (+ it 20)))
  (assert-eql 35 (awhen (find #'even '(7 5 4 3)) (+ 5 10) (+ 15 20))))

(deftest aand (conditionals)
  (assert-false (aand nil))
  (assert-false (aand t nil))
  (assert-false (aand t nil t))
  (let tab (obj a (obj a 1 b 2) b (obj a 1 b 2) c nil)
    (assert-eql 2 (aand (gethash 'a tab) (gethash 'b it)))
    (assert-false (aand (gethash 'd tab) (gethash 'b it)))
    (assert-false (aand (gethash 'c tab) (list it)))))

(deftest aand2 (conditionals)
  (assert-false (aand2 nil))
  (assert-false (aand2 t nil))
  (assert-false (aand2 t nil t))
  (let tab (obj a (obj a 1 b 2) b (obj a 1 b 2) c nil)
    (assert-eql 2 (aand2 (gethash 'a tab) (gethash 'b it)))
    (assert-false (aand2 (gethash 'd tab) (gethash 'b it)))
    (assert-equal '(()) (aand2 (gethash 'c tab) (list it)))))

(deftest iflet2 (conditionals)
  (assert-eql 15 (iflet2 x 5 (+ x 10)))
  (assert-eql 10 (iflet2 x nil 5 10))
  (assert-eql 12 (iflet2 x (find #'even '(1 6 3 7)) (* x 2)))
  (assert-eql 10 (iflet2 x (find #'even '(1 3 7)) (+ 1 1) (+ 5 5)))
  (assert-equal '(5 10) (iflet2 (x . y) (cons 5 10) (list x y)))
  (assert-equal '(5 10)
                (iflet2 (x . y) nil 10 (list 5 10) (cons x y)))
  (assert-false (iflet2 nil nil nil nil))
  (assert-equal '(()) (iflet2 x (gethash 'a (obj a nil)) (list x))))

(deftest aif2 (conditionals)
  (assert-false (aif2 nil (+ 5 5)))
  (assert-eql 7 (aif2 (find #'even '(15 2 7 8)) (+ it 5)))
  (let tab (obj a nil b 5)
    (assert-eql 15 (aif2 (gethash 'b tab) (+ it 10)))
    (assert-true (aif2 (gethash 'a tab) (not it)))
    (assert-false (aif2 (gethash 'c tab) (not it)))))

(deftest case (conditionals)
  (assert-false (case 'c a 1 b 2))
  (assert-eql 1 (case 'a a 1 b 2))
  (assert-eql 1 (case 'b (a b) 1 c 2))
  (assert-eql 3 (case 'c a 1 b 2 t 3))
  (assert-false (case 'c a 1 b 2 (t) 3))
  (assert-eql 3 (case t a 1 b 2 (t) 3 t 4)))

(deftest caselet (conditionals)
  (assert-eql 5
    (caselet x 10
      10 5
      20 30
      t  x))
  (assert-eql 30
    (caselet x 20
      10 5
      20 30
      t x))
  (assert-eql 50
    (caselet x 50
      10 5
      20 30
      t x)))

(deftest typecase (clamp)
  (assert-true
      (typecase 5
        number  t
        t       nil))
  (assert-false
      (typecase nil
        number t
        t      nil)))
