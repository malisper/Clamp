;;;; These are utilities for working with (hash) tables.

(in-package :clamp)
(use-syntax :clamp)

(def keys (tab)
  "Returns all of the keys of the table TAB."
  (ret result '()
    (maphash (fn (k v)
		 (declare (ignore v))
		 (push k result))
	     tab)))

(def vals (tab)
  "Returns all of the values stored in table TAB."
  (ret result '()
    (maphash (fn (k v)
		 (declare (ignore k))
		 (push v result))
	     tab)))

(def listtab (xs &rest args)
  "Returns a table which is equivalent to the alist XS. Takes
   additional arguments which are passed to table, specifying the
   kind of table to be created."
  (ret result (apply #'table args)
    (each (k v) xs
      (= (gethash k result) v))))

(def tablist (tab)
  "Returns an alist which is equivalent to the table TAB."
  (ret result '()
    (maphash (fn (k v) (push (list k v) result)) tab)))

(mac obj (&rest args)
  "Creates a table with every two arguments being key/value pairs.
   The keys are not evaluated."
  `(listtab (list ,@(map [let (k v) _ `(list ',k ,v)]
			 (pair args)))
	    :test #'iso))

(def alref (al key &optional (cdr nil))
  "Returns the value of KEY in the alist AL. If CDR is t, the value
   is stored in the cdr. Otherwise it is assumed it is stored in the
   cadr."
  (let pair (assoc key al)
    (values (if cdr (cdr pair) (cadr pair)) pair)))

(def counts (seq &key (test #'iso) (key #'idfn))
  "Returns a table containing how many times every element in SEQ
   appears. The procedure TEST needs to be able to be passed to table
   for creating a table."
  (ret result (table :test test)
    (each x seq
      (let val (call key x)
        (or2= (gethash val result) 0)
        (++ (gethash val result))))))

(def commonest (seq &key (test #'iso) (key #'idfn))
  "Returns the most common element in SEQ and how often it occurs."
  (with (winner nil n 0)
    (maphash
      (fn (k v)
        (when (> v n)
          (= winner k
             n v)))
      (counts seq :test test :key key))
    (values winner n)))

(def memtable (keys &key (val t) (test #'is))
  "Creates a table with all of the keys in KEYS having the value VAL."
  (ret result (table :test test)
    (each k keys
      (= (gethash k result) val))))
