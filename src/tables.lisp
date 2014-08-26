;;;; These are utilities for working with (hash) tables.

(in-package :clamp)

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

(def alref (al key)
  "Returns the value of KEY in the alist AL."
  (let pair (assoc key al)
    (values (cadr pair) pair)))

(def counts (seq &optional (test #'iso))
  "Returns a table containing how many times every element in SEQ
   appears. The function TEST needs to be able to be passed to table
   for creating a table."
  (ret result (table :test test)
    (loop for x being the elements of seq
          do
          (or2= (gethash x result) 0)
          (++ (gethash x result)))))
