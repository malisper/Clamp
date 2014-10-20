(in-package :clamp-tests)

(defsuite setforms (clamp))

(deftest zap (setforms)
  (assert-eql 100 (ret x 10 (zap [* _ _] x)))
  (assert-equal (range 1 10)
    (ret x (range 1 5)
      (zap #'append x (range 6 10)))))

(deftest or= (setforms)
  (let x nil
    (assert-eql 5 (or= x 5))
    (assert-eql 5 (or= x 7)))
  ;; This is for the problem or2= is supposed to fix.
  (let tab (table)
    (assert-eql nil (or= (gethash 'a tab) nil))
    (assert-eql 5   (or= (gethash 'a tab) 5))))

(deftest or2= (setforms)
  (let tab (table)
    (assert-eql 5   (or2= (gethash 'a tab) 5))
    (assert-eql 5   (or2= (gethash 'a tab) 7))
    (assert-eql nil (or2= (gethash 'b tab) nil))
    (assert-eql nil (or2= (gethash 'b tab) 5))))
