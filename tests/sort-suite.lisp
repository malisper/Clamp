(in-package :clamp-tests)

(defsuite sort (clamp))

(deftest compare (sort)
  (assert-true  (call (compare #'< #'len) '(1 2) '(1 2 3)))
  (assert-false (call (compare #'< #'len) '(1 2 3) '(1 2 3))))

(deftest best (sort)
  (assert-eql 5 (best #'< '(10 9 5 7 8)))
  (assert-eql 11 (best #'> '(10 9 5 7 11 4)))
  (assert-equal '(1 2) (best #'< '((1 2 3) (4 5 6) (1 2)) #'len))
  (assert-equal '(1 2 3) (best #'> '((1 2 3) (4 5) (1 2)) #'len)))
