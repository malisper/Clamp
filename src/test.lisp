;;;; several tests for clamp
;;;; TODO: figure out how to avoid duplication with packages

(defpackage :clamp-tests
  (:use :clamp :clunit)
  (:export :clamp))

(in-package :clamp-tests)

(defsuite clamp ())

(defsuite base (clamp))
(defsuite binding (clamp))
(defsuite hof (clamp))
(defsuite list (clamp))
(defsuite conditionals (clamp))
(defsuite print (clamp))

;;; base

;;; figure out how to add tests for reader macros

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

(deftest auto (base)
  (assert-true  (clamp::auto 'abc@))
  (assert-false (clamp::auto 'abc)))

(deftest if (base)
  (assert-expands (cond (a b) (c d)) (if a b c d))
  (assert-expands (cond (a b) (t c)) (if a b c)))

;;; binding

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

;;; hof

(deftest testify (hof)
  (assert-true  (funcall (testify 5) 5))
  (assert-false (funcall (testify 5) 4))
  (assert-true  (funcall (testify #'even) 4))
  (assert-false (funcall (testify #'even) 5)))

(deftest rem (hof)
  (assert-equal '() (rem 5 '()))
  (assert-equal '() (rem #'even '()))
  (assert-equal '(1 2 8 2) (rem 5 '(1 5 2 8 2 5)))
  (assert-equal '(5 29 5) (rem #'even '(2 5 29 5 28)))
  (assert-equal '() (rem #'even '(2 12 16 4)))
  (assert-equal '(13 5 7) (rem #'even '(13 5 7)))
  ;;; same tests but with vectors instead
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
  ;;; same tests but for vectors
  (assert-equalp #() (keep 7 #()))
  (assert-equalp #() (keep #'even #()))
  (assert-equalp #(2 8 2 4) (keep #'even #(1 2 8 2 3 4)))
  (assert-equalp #() (keep #'even #(5 7 3)))
  (assert-equalp #(2 12 72 6) (keep #'even #(2 12 72 6))))

;; member does not work on vectors (what is the tail of a vector?)
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
  ;;; same tests but for vectors
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
  ;;; same tests but for vectors
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
  ;;;
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
  ;;; same tests but for vectors
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

;;; list

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
  ;;; same tests but for vectors
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

(deftest n-of (list)
  (assert-equal '(1 1 1) (n-of 3 1))
  (let x 0
    (assert-equal (range 1 5) (n-of 5 (incf x)))))

(deftest drain (list)
  (assert-equal '((1 2) (3 4))
		(w/instring in "(1 2) (3 4)"
		  (drain (read in nil nil))))
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

;;; conditionals

(deftest iflet (conditionals)
  (assert-eql 15 (iflet x 5 (+ x 10)))
  (assert-eql 12 (iflet x (find #'even '(1 6 3 7)) (* x 2)))
  (assert-eql 10 (iflet x (find #'even '(1 3 7)) (+ 1 1) (+ 5 5)))
  (assert-equal '(5 10) (iflet (x . y) (cons 5 10) (list x y)))
  (assert-equal '(5 10)
                (iflet (x . y) nil 10 (list 5 10) (cons x y)))
  (assert-false (iflet nil nil nil nil)))

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
  (let tab (obj a (obj a 1 b 2) b (obj a 1 b 2))
    (assert-eql 2 (aand (gethash 'a tab) (gethash 'b it)))
    (assert-false (aand (gethash 'c tab) (gethash 'b it)))))

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


(deftest pr (print)
  (assert-equal "hello world 5"
                ;; need to use *standard-output* because pr
                ;; currently does not allow output to another stream 
                (with-output-to-string (*standard-output*)
		  (pr "hello" " world " (+ 2 3))))
  (with-output-to-string (*standard-output*)
    (assert-eql 3 (pr (+ 1 2) (+ 4 5)))))

(deftest prn (print)
  (assert-equal (format nil "~%")
                (with-output-to-string (*standard-output*)
		  (prn)))
  (assert-equal (format nil "Hello World 5~%")
                (with-output-to-string (*standard-output*)
		  (prn "Hello" " World " (+ 3 2))))
  (with-output-to-string (*standard-output*)
    (assert-eql 5 (prn (+ 1 4) (+ 3 7)))))

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
  (assert-eq 'hello (w/instring stream "Hello World" (read stream)))
  (assert-equal "Hello World" (w/instring stream "Hello World" (read-line stream)))
  (assert-equal 123 (w/instring stream "123" (parse-integer (read-line stream)))))

(deftest fromstring (print)
  (assert-eq 'hello (fromstring "Hello World" (read)))
  (assert-equal "Hello World" (fromstring "Hello World" (read-line)))
  (assert-eql 123 (fromstring "123" (parse-integer (read-line)))))
