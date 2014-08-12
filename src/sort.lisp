;;;; functions for specific kinds of sorting

(in-package :clamp)

(def compare (comparer scorer)
  "Returns a function which compares its arguments score on scorer
   with comparer. Generally should use the :key argument to other
   procedures instead"
  (fn (x y) (funcall comparer (funcall scorer x) (funcall scorer y))))

(def best (f xs &key (key #'identity))
  "Finds the first element of the list xs if it was sorted using f."
  (if (no xs)
      nil
      (ret wins (car xs)
				(let score (funcall key wins)
					(each elt (cdr xs)
						(let elt-score (funcall key elt)
							(when (funcall f elt-score score)
								(= wins elt
									 score elt-score))))))))

(def bestn (n f xs)
  "Finds the first n elements of the list xs if it was sorted using f"
  (firstn n (sort xs f)))

(def nsort (comparer sequence &optional (key #'identity))
  "Destructively sorts the sequence using comparer"
  (cl:sort sequence comparer :key key))

(def sort (comparer sequence &optional (key #'identity))
  "Non-destructively sorts sequence using comparer"
  (nsort comparer (copy-seq sequence) key))
