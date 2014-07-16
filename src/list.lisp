;;;; these are functions for working with lists

(in-package "CLAMP")

(def range (a b)
  "Generates the range of numbers from a to b"
  (if (> a b)
      '()
      (cons a (range (1+ a) b))))

(def firstn (n xs)
  "Evaluates to the first n elements of the list xs"
  (if (no n)           xs
      (and (> n 0) xs) (cons (car xs) (firstn (1- n) (cdr xs)))
      'else            '()))

(def group (xs &key (by 2) (with #'list))
  "Groups every 'by' elements using the procedure 'with'"
  (if (no xs)
      '()
      (cons (apply with (firstn by xs))
	    (group (nthcdr by xs) :by by :with with))))

(def last1 (xs)
  "Evaluates to the last element of xs. Not the last cons pair"
  (car (last xs)))

(def flat (tree)
  "A list of all of the atoms in a tree"
  (if (atom tree)
      (list tree)
      (append (flat (car tree))
	      (flat (cdr tree)))))

;;; predicates for testing length
;;; may optimize these but they need testing
;;; to see if it would make any difference

(def len< (xs n)
  "A predicate for testing if the length of xs is less than n"
  (< (len xs) n))

(def len> (xs n)
  "A predicate for testing if the length of xs is greater than n"
  (> (len xs) n))

(mac n-of (n exp)
  "Evaluates exp n times and collects the result into a list"
  ;; loop generates faster code than what I would write by hand
  `(loop repeat ,n collect ,exp))
