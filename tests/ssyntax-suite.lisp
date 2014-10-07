(in-package :experimental-tests)

(deftest notf (clamp-experimental)
  (w/ssyntax
    (assert-true  (~idfn nil))
    (assert-false (~idfn t))
    (assert-false (~not nil))
    (assert-true  (~not t))
    (assert-true  (~is 5 4))
    (assert-false (~is 5 5))
    (assert-equal '(t nil t nil t) (map ~even (range 1 5)))
    (assert-equal '(nil t nil t nil) (map ~odd (range 1 5)))
    (assert-true  (~or nil nil nil))
    (assert-false (~or nil nil t))))

(deftest andf (clamp-experimental)
  (w/ssyntax
    (assert-false (integerp&plusp -5.5))
    (assert-false (integerp&plusp 5.5))
    (assert-false (integerp&plusp -5))
    (assert-true  (integerp&plusp 4))
    (assert-equal '(nil nil nil t) (map integerp&even '(-5.5 5.5 -5 4)))
    (assert-true  (integerp&plusp&even 6))
    (assert-false (integerp&plusp&even 5.5))
    (assert-false (integerp&plusp&even -4))))

(deftest compose (clamp-experimental)
  (w/ssyntax
    (assert-equal '((5 . 10)) (list+cons 5 10))
    (assert-equal '(6)        (list+inc 5))
    (assert-equal '(((1)))    (list+list+list 1))))
