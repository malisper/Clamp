;;;; Several tests for clamp.

(defpackage :clamp-tests
  (:use :clamp :clunit)
  (:export :clamp))

(in-package :clamp-tests)

(defsuite clamp ())
(defsuite base (clamp))
(defsuite binding (clamp))
(defsuite conditionals (clamp))
(defsuite fns (clamp))
(defsuite fnops (clamp))
(defsuite hof (clamp))
(defsuite iter (clamp))
(defsuite list (clamp))
(defsuite print (clamp))
(defsuite memoize (clamp))
(defsuite misc (clamp))
(defsuite tables (clamp))

;;;; Tests for base.

(deftest map (base)
  (assert-equal  '(1 4 9) (map [* _ _] '(1 2 3)))
  (assert-equalp '(1 4 9) (map [* _ _] #(1 2 3)))
  (assert-equal  '(5 7 9) (map #'+ '(1 2 3) #(4 5 6)))
  (assert-equalp '(5 7 9) (map #'+ #(1 2 3) '(4 5 6))))

(deftest literal-fn (base)
  (assert-equal (range 1 10) (map [+ _ 2] (range -1 8))))

(deftest single (base)
  (assert-true  (single '(a)))
  (assert-false (single '()))
  (assert-false (single '(a b)))
  (assert-false (single '(a b c))))

(deftest pair (base)
  (assert-equal '((a b) (c d)) (clamp::pair '(a b c d)))
  (assert-equal '((a b) (c)) (clamp::pair '(a b c)))
  (assert-equal '(3 7) (clamp::pair '(1 2 3 4) #'+))
  (assert-equal '(3 3) (clamp::pair '(1 2 3) #'+)))

(deftest if (base)
  (assert-expands (cond (a b) (c d)) (if a b c d))
  (assert-expands (cond (a b) (t c)) (if a b c)))

;;;; Tests for binding.

(deftest with (binding)
  (assert-eql 6 (with (a 1 b 2 c 3) (+ a b c)))
  (assert-eql 6 (with ((x y) (list 1 2) z 3) (+ x y z)))
  (assert-equal '(2 1)
                (with (a 1 b 2) (with (a b b a) (list a b)))))

(deftest let (binding)
  (assert-expands (with (a b) c) (let a b c))
  (assert-eql 8 (let x 3 (+ x 5)))
  (assert-eql 3 (let (x . y) (cons 1 2) (+ x y))))

(deftest ret (binding)
  (assert-eql 15 (ret x 5 (incf x 10) nil))
  (assert-equal '(a b) (ret x '() (push 'b x) (push 'a x) nil)))

(deftest flet1 (binding)
  (assert-expands (flet ((a (x y z) b))) (flet1 a (x y z) b)))

(deftest withs (binding)
  (assert-eql 12 (withs (x 5 y (+ x 3)) (+ y 4))))

;;;; Tests for hof.

(deftest testify (hof)
  (assert-true  (call (testify 5) 5))
  (assert-false (call (testify 5) 4))
  (assert-true  (call (testify #'even) 4))
  (assert-false (call (testify #'even) 5)))

(deftest rem (hof)
  (assert-equal '() (rem 5 '()))
  (assert-equal '() (rem #'even '()))
  (assert-equal '(1 2 8 2) (rem 5 '(1 5 2 8 2 5)))
  (assert-equal '(5 29 5) (rem #'even '(2 5 29 5 28)))
  (assert-equal '() (rem #'even '(2 12 16 4)))
  (assert-equal '(13 5 7) (rem #'even '(13 5 7)))
  ;; Same tests but with vectors instead.
  (assert-equalp #() (rem 5 #()))
  (assert-equalp #() (rem #'even #()))
  (assert-equalp #(1 2 8 2) (rem 5 #(1 5 2 8 2 5)))
  (assert-equalp #(5 29 5) (rem #'even #(2 5 29 5 28)))
  (assert-equalp #() (rem #'even #(2 12 16 4)))
  (assert-equalp #(13 5 7) (rem #'even #(13 5 7))))

(deftest keep (hof)
  (assert-equal '() (keep 7 '()))
  (assert-equal '() (keep #'even '()))
  (assert-equal '(2 8 2 4) (keep #'even '(1 2 8 2 3 4)))
  (assert-equal '() (keep #'even '(5 7 3)))
  (assert-equal '(2 12 72 6) (keep #'even '(2 12 72 6)))
  ;; Same tests but for vectors.
  (assert-equalp #() (keep 7 #()))
  (assert-equalp #() (keep #'even #()))
  (assert-equalp #(2 8 2 4) (keep #'even #(1 2 8 2 3 4)))
  (assert-equalp #() (keep #'even #(5 7 3)))
  (assert-equalp #(2 12 72 6) (keep #'even #(2 12 72 6))))

;;; Member does not work on vectors (what is the tail of a vector?).
(deftest mem (hof)
  (assert-false (mem 7 '()))
  (assert-false (mem #'even '()))
  (assert-false (mem 3 '(1 29 32 5)))
  (assert-equal '(5 3 2) (mem 5 '(1 6 3 5 3 2)))
  (assert-equal '(2 3) (mem #'even '(1 9 2 3))))

(deftest find (hof)
  (assert-false (find 5 '()))
  (assert-false (find #'even '()))
  (assert-false (find 5 '(2 9 1 2 7 3)))
  (assert-eql 5 (find 5 '(1 3 5 2 9 3)))
  (assert-eql 2 (find #'even '(1 3 5 2 9 3 4 6 7)))
  ;; Same tests but for vectors.
  (assert-false (find 5 #()))
  (assert-false (find #'even #()))
  (assert-false (find 5 #(2 9 1 2 7 3)))
  (assert-eql 5 (find 5 #(1 3 5 2 9 3)))
  (assert-eql 2 (find #'even #(1 3 5 2 9 3 4 6 7))))

(deftest count (hof)
  (assert-eql 0 (count 2 '()))
  (assert-eql 0 (count #'even '()))
  (assert-eql 0 (count #'even '(1 3 71 21)))
  (assert-eql 3 (count 5 '(1 5 3 2 5 7 5)))
  (assert-eql 4 (count #'even '(1 6 3 2 2 4)))
  ;; Same tests but for vectors.
  (assert-eql 0 (count 2 #()))
  (assert-eql 0 (count #'even #()))
  (assert-eql 0 (count #'even #(1 3 71 21)))
  (assert-eql 3 (count 5 #(1 5 3 2 5 7 5)))
  (assert-eql 4 (count #'even #(1 6 3 2 2 4))))

(deftest pos (hof)
  (assert-false (pos 2 '()))
  (assert-false (pos #'even '()))
  (assert-false (pos #'even '(123 45 3 7)))
  (assert-eql 2 (pos 5 '(1 3 5 3 2 5)))
  (assert-eql 3 (pos #'even '(1 7 3 2 5 7 4 2)))
  ;; Same tests but for vectors.
  (assert-false (pos 2 #()))
  (assert-false (pos #'even #()))
  (assert-false (pos #'even #(123 45 3 7)))
  (assert-eql 2 (pos 5 #(1 3 5 3 2 5)))
  (assert-eql 3 (pos #'even #(1 7 3 2 5 7 4 2))))

(deftest mappend (hof)
  (assert-equal '() (mappend #'identity '()))
  (assert-equal '(1 4 2 5 3 6) (mappend #'list '(1 2 3) '(4 5 6))))

(deftest partition (hof)
  (assert-equal '(() ()) (mvl (partition #'even '())))
  (assert-equal '(() ()) (mvl (partition 1 '())))
  (assert-equal '((2 4) (1 3 5)) (mvl (partition #'even '(1 2 3 4 5))))
  (assert-equal '((4) (5)) (mvl (partition #'even '(1 2 3 4 5) :start 3)))
  (assert-equal '((1 1 1) (0)) (mvl (partition 1 '(1 0 1 1))))
  (assert-equal '(((2) (4)) ((1) (3) (5)))
		(mvl (partition #'even '((1) (2) (3) (4) (5)) :key #'car)))
  (assert-equal '(((4)) ((5)))
		(mvl (partition #'even '((1) (2) (3) (4) (5))
				:key #'car
				:start 3)))
  ;; Same tests but for vectors.
  (assert-equal '(() ()) (mvl (partition #'even #())))
  (assert-equal '(() ()) (mvl (partition 1 #())))
  (assert-equal '((2 4) (1 3 5)) (mvl (partition #'even #(1 2 3 4 5))))
  (assert-equal '((4) (5)) (mvl (partition #'even #(1 2 3 4 5) :start 3)))
  (assert-equal '((1 1 1) (0)) (mvl (partition 1 #(1 0 1 1))))
  (assert-equal '(((2) (4)) ((1) (3) (5)))
		(mvl (partition #'even #((1) (2) (3) (4) (5)) :key #'car)))
  (assert-equal '(((4)) ((5)))
		(mvl (partition #'even #((1) (2) (3) (4) (5))
				:key #'car
				:start 3))))

(deftest trues (hof)
  (let alist '((a 1) (b 2) (c 3))
    (assert-equal '((c 3) (a 1))
                  (trues [assoc _ alist]
                         '(c d a)))))

;;;; Tests for list.

(deftest range (list)
  (assert-equal '(1 2 3 4 5) (range 1 5))
  (assert-equal '(5) (range 5 5))
  (assert-equal '() (range 5 4))
  (assert-equal '(2 4 6 8 10) (range 2 10 2))
  (assert-equal '(1 3 5 7 9) (range 1 10 2)))

(deftest firstn (list)
  (assert-equal '(1 2 3) (firstn 3 (range 1 5)))
  (assert-equal nil (firstn 0 (range 1 5)))
  (assert-equal (range 1 5) (firstn nil (range 1 5)))
  (assert-equal (range 1 5) (firstn 10 (range 1 5)))
  (assert-equal (range 1 5) (firstn 5 (vector 1 2 3 4 5 6 7 8)))
  (assert-equal (range 1 5) (firstn 10 (vector 1 2 3 4 5))))

(deftest split (list)
  (assert-equal '(() ()) (mvl (split '() 0)))
  (assert-equal '(() (a b c)) (mvl (split '(a b c) 0)))
  (assert-equal '((a) (b c)) (mvl (split '(a b c) 1)))
  ;; Same tests but for vectors.
  (assert-equalp '(#() #()) (mvl (split #() 0)))
  (assert-equalp '(#() #(a b c)) (mvl (split #(a b c) 0)))
  (assert-equalp '(#(a) #(b c)) (mvl (split #(a b c) 1))))

(deftest group (list)
  (assert-equal '() (group '()))
  (assert-equal '() (group '() :by 3))
  (assert-equal '() (group '() :with #'+))
  (assert-equal '((1 2) (3 4)) (group '(1 2 3 4)))
  (assert-equal '((1 2) (3 4) (5)) (group (range 1 5)))
  (assert-equal '((1 2 3) (4 5)) (group (range 1 5) :by 3))
  (assert-equal '(6 9) (group (range 1 5) :by 3 :with #'+))
  (assert-equal '(3 7 11) (group (range 1 6) :with #'+)))

(deftest last1 (list)
  (assert-eql 10 (last1 (range 1 10)))
  (assert-eql 'c (last1 '(a b c))))

(deftest flat (list)
  (assert-equal (range 1 5) (flat '(((1) 2) (3 4) 5)))
  (assert-equal (range 1 5) (flat (range 1 5)))
  (assert-equal (range 1 5) (flat '(((1 2 3 4 5))))))

(deftest len< (list)
  (assert-true  (len< '(1 2 3) 4))
  (assert-false (len< '(1 2 3) 3)
  (assert-false (len< '(1 2 3) 2))))

(deftest len> (list)
  (assert-true  (len> '(1 2 3) 2))
  (assert-false (len> '(1 2 3) 3))
  (assert-false (len> '(1 2 3) 4)))

(deftest n-of (list)
  (assert-equal '(1 1 1) (n-of 3 1))
  (let x 0
    (assert-equal (range 1 5) (n-of 5 (incf x)))))

(deftest drain (list)
  (assert-equal '((1 2) (3 4))
		(w/instring in "(1 2) (3 4)"
		  (drain (read :from in :eof nil))))
  (assert-equal '(128 64 32 16 8 4 2)
		(let x 256
		  (drain (= x (/ x 2)) 1)))
  (assert-equal '(100 50)
                (let x 200
                  (drain (= x (/ x 2)) #'odd))))

(deftest caris (list)
  (assert-false (caris 5 5))
  (assert-false (caris '(1 2 3) 2))
  (assert-true  (caris '(1 2 3) 1)))

(deftest carif (list)
  (assert-eql 5 (carif 5))
  (assert-eql 1 (carif '(1 2 3))))

(deftest consif (list)
  (assert-equal '(1 2 3) (consif 1 '(2 3)))
  (assert-equal '(2 3)   (consif nil '(2 3))))

(deftest conswhen (list)
  (assert-equal '(1 2 3) (conswhen #'idfn 1 '(2 3)))
  (assert-equal '(2 3)   (conswhen #'even 1 '(2 3)))
  (assert-equal '(1 2 3) (conswhen #'odd  1 '(2 3))))

(deftest cars (list)
  (assert-equal '() (cars '()))
  (assert-equal '(1 4 7) (cars '((1 2 3) (4 5 6) (7 8 9)))))

(deftest cdrs (list)
  (assert-equal '() (cdrs '()))
  (assert-equal '((2 3) (5 6) (8 9)) (cdrs '((1 2 3) (4 5 6) (7 8 9)))))

;;;; Test for conditionals.

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

;;;; Tests for print.

(deftest pr (print)
  (assert-equal "hello world 5"
                (tostring (pr "hello" " world " (+ 2 3))))
  ;; This use of tostring is just so the output is not visible.
  (tostring (assert-eql 3 (pr (+ 1 2) (+ 4 5)))))

(deftest prn (print)
  (assert-equal (format nil "~%")
                (tostring (prn)))
  (assert-equal (format nil "Hello World 5~%")
                (tostring (prn "Hello" " World " (+ 3 2))))
  ;; This use of tostring is just so the output is not visible.
  (tostring (assert-eql 5 (prn (+ 1 4) (+ 3 7)))))

(deftest w/outstring (print)
  (assert-equal "Hello World 3" (w/outstring stream
				  (princ "Hello " stream)
				  (princ "World " stream)
				  (princ (+ 1 2) stream)))
  (assert-equal "" (w/outstring stream)))

(deftest tostring (print)
  (assert-equal "Hello World 3" (tostring (pr "Hello " "World " (+ 1 2))))
  (assert-equal "" (tostring))
  (assert-equal (format nil "~%") (tostring (prn))))

(deftest w/instring (print)
  (assert-eq 'hello (w/instring stream "Hello World" (read :from stream)))
  (assert-equal "Hello World" (w/instring stream "Hello World" (read-line :from stream)))
  (assert-equal 123 (w/instring stream "123" (parse-integer (read-line :from stream)))))

(deftest fromstring (print)
  (assert-eq 'hello (fromstring "Hello World" (read)))
  (assert-equal "Hello World" (fromstring "Hello World" (read-line)))
  (assert-eql 123 (fromstring "123" (parse-integer (read-line)))))

;;;; Test for Fns.

(deftest rfn (fns)
  (let f (rfn fib (n)
           (if (<= 0 n 1)
               n
               (+ (fib (- n 1))
                  (fib (- n 2)))))
     (assert-eql 55 (call f 10))
     (assert-eql 34 (call f 9))))

(deftest afn (fns)
  (let f (afn (n)
           (if (<= 0 n 1)
               n
               (+ (self (- n 1))
                  (self (- n 2)))))
     (assert-eql 55 (call f 10))
     (assert-eql 34 (call f 9))))

;;;; Tests for fnops.

(deftest compose (fnops)
  (assert-eql 5 (call (compose #'1+ #'length) '(1 2 3 4)))
  (assert-equal '(5) (call (compose #'list #'1-) 6)))

(deftest fif (fnops)
  (assert-eql 5 (call (fif) 5))
  (assert-eql 6 (call (fif #'odd #'1+ #'1-) 5))
  (assert-eql 5 (call (fif #'odd #'1+ #'1-) 6))
  (assert-eql 0 (call (fif #'plusp #'1+ #'minusp #'1-) 0))
  (assert-eql 2 (call (fif #'plusp #'1+ #'minusp #'1-) 1))
  (assert-eql -2 (call (fif #'plusp #'1+ #'minusp #'1-) -1)))

(deftest andf (fnops)
  (assert-true  (call (andf #'integerp #'even) 4))
  (assert-false (call (andf #'integerp #'even) 3.5))
  (assert-false (call (andf #'integerp #'even) 3))
  (assert-eql 5 (call (andf #'integerp #'even #'1+) 4))
  (assert-true  (call (andf #'> #'multiple) 10 5)))

(deftest orf (fnops)
  (assert-true  (call (orf #'even #'plusp) 4))
  (assert-true  (call (orf #'even #'plusp) 3))
  (assert-true  (call (orf #'even #'plusp) -2))
  (assert-false (call (orf #'even #'plusp) -3))
  (assert-false (call (orf #'> #'<) 5 5)))

(deftest curry (fnops)
  (assert-eql 15 (call (curry #'+ 5) 10))
  (assert-eql 75 (call (curry #'+ 5 10 15) 20 25))
  (assert-eql 55 (call (curry #'reduce #'+) (range 1 10))))

(deftest rcurry (fnops)
  (assert-eql 15 (call (rcurry #'+ 5) 10))
  (assert-eql 75 (call (rcurry #'+ 5 10 15) 20 25))
  (assert-eql 55 (call (rcurry #'reduce (range 1 10)) #'+)))

;;;; Tests for iter.

(deftest rec (iter)
  (assert-eql 55
      (rec (n 10)
        (if (<= 0 n 1)
            n
            (+ (recur (- n 1))
               (recur (- n 2))))))
  (assert-eql 120
      (rec (n 5)
        (if (is n 0)
            1
            (* n (recur (- n 1))))))
  (assert-eql 720
      (rec (n 6 acc 1)
        (if (is n 0)
            acc
            (recur (- n 1) (* acc n))))))

(deftest repeat (iter)
  (assert-equal (n-of 10 5) (accum a (repeat 10 (a 5))))
  (assert-eql   1024 (ret result 1 (repeat 10 (zap #'* result 2))))
  (assert-eql   625  (ret result 5 (repeat 2 (zap [* _ _] result))))
  (assert-eql   55   (ret result 0
                       (let i 0
                         (repeat 10
                           (++ i)
                           (++ result i))))))

(deftest up (iter)
  (assert-equal (range 1 9) (accum a (up i 1 10 (a i))))
  (assert-eql   45 (ret result 0 (up i 1 10 (++ result i))))
  (assert-eql   20 (ret result 0 (up i 1 6 (++ result i) (++ result)))))

(deftest upto (iter)
  (assert-equal (range 1 10) (accum a (upto i 1 10 (a i))))
  (assert-eql   55 (ret result 0 (upto i 1 10 (++ result i))))
  (assert-eql   20 (ret result 0 (upto i 1 5 (++ result i) (++ result)))))

(deftest down (iter)
  (assert-equal (rev (range 1 9)) (accum a (down i 10 1 (a i))))
  (assert-eql  45 (ret result 0 (down i 10 1 (++ result i))))
  (assert-eql  20 (ret result 0 (down i 6 1 (++ result i) (++ result)))))

(deftest downfrom (iter)
  (assert-equal (rev (range 1 10)) (accum a (downfrom i 10 1 (a i))))
  (assert-eql   55 (ret result 0 (downfrom i 10 1 (++ result i))))
  (assert-eql   20 (ret result 0 (downfrom i 5 1 (++ result i) (++ result)))))

(deftest while (iter)
  (assert-equal '(t t t) (accum a
                            (fromstring "10 4 6 7 8"
                              (while (even (read))
                                (a t)))))
  (assert-eq 100 (ret result 0
                   (let counter 10
                     (while (> counter 0)
                       (++ result 10)
                       (-- counter 1))))))

(deftest until (iter)
  (assert-equal '(t t t) (accum a
                           (fromstring "10 4 6 7 8"
                             (until (odd (read))
                               (a t)))))
  (assert-eq 100 (ret result 0
                   (let counter 10
                     (until (<= counter 0)
                       (++ result 10)
                       (-- counter 1))))))

(deftest each (iter)
  (assert-equal (map [* _ _] (range 1 10))
                (accum a
                  (each x (range 1 10)
                    (a (* x x)))))
  (assert-eql 54
              (ret result 0
                (each x (range 1 9)
                  (++ result x)
                  (++ result)))))

(deftest on (iter)
  (assert-equal '((0 a) (1 b) (2 c))
                (accum a
                  (on x '(a b c)
                    (a (list index x)))))
  (assert-eql 110
              (ret result 0
                (on x (range 0 10)
                  (++ result x)
                  (++ result index)))))

(deftest whilet (iter)
  (assert-equal '(1 2 3) (accum a (fromstring "1 2 3"
                                    (whilet x (read :eof nil)
                                      (a x))))))

(deftest whiler (iter)
  (assert-equal '(1 2 3)
                (accum a (fromstring "1 2 3"
                  (whiler x (read :eof nil) nil
                    (a x)))))
  (assert-equal '(1 2 3)
                (accum a (fromstring "1 2 3"
                           (whiler x (read :eof t) t
                             (a x))))))

(deftest forlen (iter)
  (let xs '(1 2 3)
    (assert-equal (rev xs) (ret result '()
                             (forlen i xs
                               (push (elt xs i) result)))))
  (let seq #(1 2 3)
    (assert-equal (rev (coerce seq 'list))
                  (ret result '()
                    (forlen i seq
                      (push (elt seq i) result))))))

;;;; Tests for memoize.

;; This serves as a test for memo and defmemo.
(deftest defmemo (memoize)
  (defmemo fib (n)
    (if (<= 0 n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))
  ;; We know defmemo works if this ever finishes with the answer.
  (assert-eql 354224848179261915075 (fib 100)))

;;;; Tests for misc.

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

(deftest keys (tables)
  (let tab (table)
    (assert-equal '() (keys tab))
    (= (gethash 'a tab) 1)
    (assert-equal '(a) (keys tab))
    (= (gethash 'b tab) 2)
    (assert-true (or (equal '(a b) (keys tab))
                     (equal '(b a) (keys tab))))))

(deftest vals (tables)
  (let tab (table)
    (assert-equal '() (vals tab))
    (= (gethash 'a tab) 1)
    (assert-equal '(1) (vals tab))
    (= (gethash 'b tab) 2)
    (assert-true (or (equal '(1 2) (vals tab))
                     (equal '(2 1) (vals tab))))))

(deftest listtab (tables)
  (withs (tab (listtab '((a 1) (b 2)))
          keys (keys tab)
          vals (vals tab))
    (assert-true (or (equal '(a b) keys)
                     (equal '(b a) keys)))
    (assert-true (or (equal '(1 2) vals)
                     (equal '(2 1) vals)))
    (assert-eql 1 (gethash 'a tab))
    (assert-eql 2 (gethash 'b tab))))

(deftest tablist (tables)
  (let alist (tablist (obj a 1 b 2 c 3))
    (assert-eql 1 (alref alist 'a))
    (assert-eql 2 (alref alist 'b))
    (assert-eql 3 (alref alist 'c))))

(deftest obj (tables)
  (let tab (obj a 1 b 2 c 3)
    (assert-eql 1 (gethash 'a tab))
    (assert-eql 2 (gethash 'b tab))
    (assert-eql 3 (gethash 'c tab))))

(deftest alref (tables)
  (let alist '((a 1) (b 2) (c 3) (d nil))
    (assert-eql 1 (alref alist 'a))
    (assert-eql 2 (alref alist 'b))
    (assert-eql 3 (alref alist 'c))
    (assert-eql 4 (aif2 (alref alist 'd)
                        4
                        5))
    (assert-eql 5 (aif2 (alref alist 'e)
                        4
                        5))))

(deftest counts (tables)
  (let tab (counts '(1 2 3 2 1 2 3 1 2))
    (assert-eql 3 (gethash 1 tab))
    (assert-eql 4 (gethash 2 tab))
    (assert-eql 2 (gethash 3 tab)))
  (let tab (counts '((1 2) (3 4) (1 2) (1 3)) :test #'iso)
    (assert-eql 2 (gethash '(1 2) tab)))
  (let tab (counts '((1 2) (3 4) (1 2 3) (7 8 9)) :key #'len)
    (assert-eql 2 (gethash 2 tab))
    (assert-eql 2 (gethash 3 tab)))
  (let tab (counts '((1 2 3) (1 3) (7 8 3)) :test #'iso :key [rem #'even _])
    (assert-eql 2 (gethash '(1 3) tab))
    (assert-eql 1 (gethash '(7 3) tab))))

(deftest commonest (tables)
  (assert-equal '(nil 0) (mvl (commonest '())))
  (assert-equal '(5 3) (mvl (commonest'(1 6 5 2 3 4 5 9 2 8 5 9 0))))
  (assert-equal '(2 2) (mvl (commonest '((1 2) (1 2 3) (3 4) (3 4 5))
                                       :key #'len)))
  (assert-equal '((1 2) 2) (mvl (commonest '((1 2) (1 2 3) (1 2) (3 4))
                                           :test #'iso)))
  (assert-equal '((1 3) 2) (mvl (commonest '((1 2 3) (7 8 3) (1 3))
                                           :test #'iso
                                           :key [rem #'even _]))))

(deftest memtable (tables)
  (assert-equalp (table) (memtable '()))
  ;; The macro obj returns a table that uses equalp.
  (assert-equalp (obj a t b t) (memtable '(a b) :test #'equalp))
  (assert-equalp (obj a nil b nil) (memtable '(a b) :val nil :test #'equalp)))
