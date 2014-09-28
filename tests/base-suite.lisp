(in-package :clamp-tests)

(defsuite base (clamp))

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
