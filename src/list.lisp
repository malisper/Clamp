;;;; These are utilities for working with lists.

(in-package :clamp)

(def range (a b &optional (by 1))
  "Returns a list of numbers from A to B (inclusive) in steps of BY. 
   The argument BY has to be a positive integer."
  ;; The loop macro generates code that is more efficent than what
  ;; should be written by hand
  (loop for i from a to b by by collect i))

(def firstn (n seq)
  "Returns a list of the first N elements of the sequence SEQ or a
   list of all the elements if SEQ is too short. If N is nil, returns
   the entire sequence."
  (if (no n)
      seq
      (loop repeat n
	    ;; Cannot use cut to access the the elements because
            ;; this should not throw an error when the sequence
            ;; is too short.
	    for x being the elements of seq
	    collect x)))

(def split (seq n)
  "Given a sequence and an integer will return two sequences. The first
   one will contain the first N elements of the sequence, and the second
   will contain the rest of the elements of the initial sequence. The
   return sequences are of the same type as the sequence passed in."
  (values (cut seq 0 n) (cut seq n)))

(def group (xs &key (by 2) (with #'list))
  "Groups every BY elements of the given list using the procedure 
   WITH."
  (if (no xs)
      '()
      (cons (apply with (firstn by xs))
	    (group (nthcdr by xs) :by by :with with))))

(def last1 (xs)
  "Returns the last element of XS. Not the last cons pair."
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
  "Is this sequence shorter than some length?"
  (< (len seq) n))

(def len> (seq n)
  "Is this sequence longer than some length?"
  (> (len seq) n))

(mac n-of (n exp)
  "Returns a list of calling EXP, N times."
  ;; Loop generates faster code than what I would write by hand.
  `(loop repeat ,n collect ,exp))

(mac drain (exp &optional (endval nil))
  "Repeatedly evaluates EXP until it passes the testified version of
   ENDVAL. Then return a list of the results."
  (w/uniq (gval gtest)
    `(loop with ,gtest = (testify ,endval)
           for ,gval = ,exp
           until (funcall ,gtest ,gval)
           collect ,gval)))

(def caris (x val)
  "Is X a cons pair, and is its car the given value?"
  (and (consp x) (is (car x) val)))

(def carif (x)
  "Returns X if it is an atom, otherwise returns (car X)."
  (if (atom x)
      x
      (car x)))
