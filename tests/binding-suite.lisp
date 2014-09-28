(in-package :clamp-tests)

(defsuite binding (clamp))

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
