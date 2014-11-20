;;;; These are utilities for working with lists.

(in-package :clamp)

(def mklist (x)
  "If X is a list, return it. Otherwise return a list containing X."
  (if (listp x) x (list x)))

(def dotted (x)
  "Is this a dotted list?"
  (and (listp x)
       (rec (rest x)
         (if (null rest)
             nil
             (or (atom rest)
                 (recur (cdr rest)))))))

(def proper (x)
  "Is this a proper list?"
  (and (listp x)
       (not (dotted x))))

(def range (a b &optional (by 1))
  "Returns a list of numbers from A to B (inclusive) in steps of BY. 
   The argument BY has to be a positive integer."
  ;; The loop macro generates code that is more efficent than what
  ;; should be written by hand.
  (if (< a b)
      (loop for i from a to b by by collect i)
      (loop for i downfrom a to b by by collect i)))

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
	    for x in (coerce seq 'list)
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

(def last (xs)
  "Returns the last element of XS. Not the last cons pair."
  (car (lastcons xs)))

(def flat (tree)
  "Returns a list of all of the atoms in a tree (not including nil)"
  (rec (left tree acc '())
    (if (null left)
          acc
        (atom left)
          (cons left acc)
        :else
          (recur (car left)
                 (recur (cdr left)
                        acc)))))

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
   ENDVAL. Then returns a list of the results."
  (w/uniq (gval gtest)
    `(loop with ,gtest = (testify ,endval)
           for ,gval = ,exp
           until (call ,gtest ,gval)
           collect ,gval)))

(def caris (x val)
  "Is X a cons pair, and is its car the given value?"
  (and (consp x) (is (car x) val)))

(def carif (x)
  "Returns X if it is an atom, otherwise returns (car X)."
  (if (atom x)
      x
      (car x)))

(def conswhen (f x y)
  "Cons X and Y if (F X) is non-nil. Otherwise return Y."
  (if (call f x)
      (cons x y)
      y))

(def consif (x y)
  "Cons X and Y if X is non-nil. Otherwise return Y."
  (conswhen #'idfn x y))

(def cars (seq)
  "Returns a list of the cars of each list within a given sequence."
  (map #'car seq))

(def cdrs (seq)
  "Returns a list of the cdrs of each list within a given sequence."
  (map #'cdr seq))

(defgeneric get (obj arg)
  (:documentation "Returns whatever is associated with ARG in OBJ."))

(defgeneric (setf get) (val obj arg)
  (:documentation "Sets ARG to be associated with VAL in OBJ."))

(defmethod get ((seq sequence) (n number))
  "Returns the Nth element of a sequence."
  (elt seq n))

(defmethod (setf get) (val (seq sequence) (n number))
  "Sets the Nth element of SEQ to VAL."
  (= (elt seq n) val))

(defmethod get ((tab hash-table) x)
  "Returns whatever is stored in TAB under X."
  (gethash x tab))

(defmethod (setf get) (val (tab hash-table) x)
  "Sets VAL to be stored under X in TAB."
  (= (gethash x tab) val))

(defmethod get (obj x)
  "Calls X on OBJECT."
  (call x obj))

;; A setter for the default case would have to lookup the setter
;; for the given argument.

(mac trav (x &rest fs)
  "Traverse X, calling fs in sequence. The symbol 'recur' is bound to
   a procedure which can be used to recursively traverse the object.
   The return value is nil."
  (w/uniq g
    `(rec (,g ,x)
       (when ,g
         ,@(map (fn (f) `(call ,f ,g)) fs)))))

(def intersperse (x ys)
  "Returns a list with the element X in between every element in YS."
  (and ys
       (cons (car ys)
             (loop for y in (cdr ys)
                   collect x
                   collect y))))
