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

(deftest bestn (sort)
  (assert-equal '(1 2 3) (bestn 3 #'< '(6 1 3 5 9 2 6)))
  (assert-equal '(6 5 4) (bestn 3 #'> '(1 5 4 3 2 6)))
  (assert-equal '(() (1)) (bestn 2 #'< '((1 2) () (1 2 3) (1)) #'len)))

(deftest sort (sort)
  (assert-equal '(1 2 3) (sort #'< '(3 1 2)))
  (assert-equal '(1 2 3 4 5 6) (sort #'< '(5 2 3 4 1 6)))
  (assert-equal '(() (1) (1 2)) (sort #'< '((1 2) () (1)) #'len)))

(deftest nsort (sort)
  (assert-equal '(1 2 3) (nsort #'< (list 3 1 2)))
  (assert-equal '(1 2 3 4 5 6) (nsort #'< (list 5 2 3 4 1 6)))
  (assert-equal '(() (1) (1 2)) (nsort #'< (list '(1 2) '() '(1)) #'len)))

