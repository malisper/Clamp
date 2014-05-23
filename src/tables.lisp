;;;; these are utilities for working with (hash) tables

(def keys (tab)
  "Evaluates to all of the keys in the hash table tab"
  (ret result '()
    (maphash (fn (k v)
		 (declare (ignore v))
		 (push k result))
	     tab)))

(def vals (tab)
  "Evaluatates to all of the values in the table tab"
  (ret result '()
    (maphash (fn (k v)
		 (declare (ignore k))
		 (push v result))
	     tab)))

(def listtab (xs)
  "Evaluates to a table which is equivalent to the alist xs"
  (ret result (table)
    (each (k v) xs
      (setf (gethash k result) v))))

(def tablist (tab)
  "Evaluates to an alist which is equivalent to the table xs"
  (ret result '()
    (maphash (fn (k v) (push (list k v) result)) tab)))

(mac obj (&rest args)
  "Makes a table for all of the passed in keys and values"
  `(listtab (list ,@(mapf [let1 (k v) _ `(list ',k ,v)]
			  (pair args)))))

(def alref (al key)
  "Evaluates to the value of key in the alist al"
  (cadr (assoc key al)))
