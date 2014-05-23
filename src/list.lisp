;;;; these are functions for working with lists

(def firstn (n xs)
  "Evaluates to the first n elements of the list xs"
  (lf (no n)           xs
      (and (> n 0) xs) (cons (car xs) (firstn (1- n) (cdr xs)))
      'else            nil))

(def last1 (xs)
  (car (last xs)))

;;; predicates for testing length
;;; may optimize these but they need testing
;;; to see if it would make any difference

(def len< (xs n)
  "A predicate for testing if the length of xs is less than n"
  (< (len xs) n))

(def len> (xs n)
  "A predicate for testing if the length of xs is greater than n"
  (> (len xs) n))
