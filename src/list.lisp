;;;; these are functions for working with lists

(in-package :clamp)

(def range (a b &optional (by 1))
  "Returns a list of numbers from a to b in steps of by. By has to be
   a positive integer."
  ;; loop generates efficent code
  (loop for i from a to b by by collect i))

(def firstn (n seq)
  "Returns a list of the first n elements of the sequence seq or a
   list of all the elements if seq is too short. Can also be given
   nil instead of a number to just return the sequence instead."
  (if (no n)
      seq
      (loop repeat n
	    ;; needed because taking the firstn elements on a larger
	    ;; list would then be an error
	    for x being the elements of seq
	    collect x)))

(def split (seq n)
  "Given a sequence and an integer will return two sequences. The first
   one will contain the first n elements of the sequence, and the second
   will contain the rest of the elements of the initial sequence."
  (values (cut seq 0 n) (cut seq n)))

(def group (xs &key (by 2) (with #'list))
  "Groups every 'by' elements of the given list using the procedure 
   'with'."
  (if (no xs)
      '()
      (cons (apply with (firstn by xs))
	    (group (nthcdr by xs) :by by :with with))))

(def last1 (xs)
  "Returns the last element of xs. Not the last cons pair."
  (car (last xs)))

(def flat (tree)
  "Returns a list of all of the atoms in a tree (not including nil)"
  (if (null tree)
        '()
      (atom tree)
        (list tree)
      (append (flat (car tree))
	      (flat (cdr tree)))))

;;; This are predicates for testing the length of sequences. They may
;;; be further optimized, but benchmarks would be needed before then.

(def len< (seq n)
  "Is a sequence shorter than some length?"
  (< (len seq) n))

(def len> (seq n)
  "Is a sequence longer than some length?"
  (> (len seq) n))

(mac n-of (n exp)
  "Returns a list of calling exp, n times."
  ;; loop generates faster code than what I would write by hand
  `(loop repeat ,n collect ,exp))

(mac drain (exp &optional (endval nil))
  "Repeatedly evaluates exp until it passes the testified version of
   endval. Then return a list of the results a list of the results."
  (w/uniq (gval gend)
    `(loop with ,gend = (testify ,endval)
           for ,gval = ,exp
           until (funcall ,gend ,gval)
           collect ,gval)))

(def caris (x val)
  "Is x a cons pair, and is its car the given value?"
  (and (consp x) (is (car x) val)))

(def carif (x)
  "Returns x if it is an atom, otherwise returns (car x)."
  (if (atom x)
      x
      (car x)))
