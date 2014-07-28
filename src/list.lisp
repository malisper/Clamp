;;;; these are functions for working with lists

(in-package "CLAMP")

(def range (a b &optional (by 1))
  "Generates the range of numbers from a to b in steps of 'by'.
   Right now 'by' has to be a positive integer instead of any
   integer"
  ;; loop generates efficent code
  (loop for i from a to b by by collect i))

(def firstn (n xs)
  "Evaluates to a list containing first n elements of the seq xs.
   If n is nil, returns the entire seq"
  (if (no n)
      xs
      (loop repeat n
	    for x being the elements of xs
	    collect x)))

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

(def caris (x val)
  "Tests that x is a cons and (car x) is val."
  (and (consp x) (is (car x) val)))

(def carif (x)
  "Returns x if it is an atom, otherwise returns (car x)"
  (if (atom x)
      x
      (car x)))
