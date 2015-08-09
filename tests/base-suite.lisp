(in-package :clamp-tests)
(use-syntax :clamp)

(defsuite base (clamp))

(deftest map (base)
  (assert-true
     (check-it (generator (list (integer)))
               (lambda (xs)
                 (is (len xs) (len (map [* _ _] xs))))))
  (assert-true
     (check-it (generator (list (integer)))
               (lambda (xs)
                 (every (fn (x y)
                          (is (* x x) y))
                        xs
                        (map [* _ _] xs))))))

(deftest literal-fn (base)
  (assert-true
    (check-it (generator (list (integer)))
              (lambda (xs)
                (every (fn (x y)
                         (is y (+ x 2)))
                       xs
                       (map [+ _ 2] xs))))))

(deftest single (base)
  (assert-true
    (check-it (generator (list (integer) :length 1))
              #'single))
  (assert-true
      (check-it (generator (list (integer) :min-length 2))
                (complement #'single)))
  (assert-false (single '())))

(deftest pair (base)
  (assert-true
    (check-it (generator (list (integer)))
              (lambda (xs)
                (is (len (clamp::pair xs))
                    (ceiling (len xs) 2))))
    (check-it (generator (list (integer)))
              (lambda (xs)
                (iso (flat (clamp::pair xs)))))))

(deftest if (base)
  (assert-expands (cond (a b) (c d)) (if a b c d))
  (assert-expands (cond (a b) (t c)) (if a b c)))
