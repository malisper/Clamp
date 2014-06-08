;;;; functions for specific kinds of sorting

(in-package "CLAMP")

(def compare (comparer scorer)
  "Returns a function which compares its arguments score on scorer
   with comparer. Generally should use the :key argument to other
   functions instead"
  (fn (x y) (funcall comparer (funcall scorer x) (funcall scorer y))))

(def best (f seq)
  "Finds the first element of seq if it was sorted using f"
  (if (no seq)
      nil
      (ret wins (car seq)
	(each elt (cdr seq)
	  (if (funcall f elt wins) (setf wins elt))))))

(def bestn (n f seq)
  "Finds the first n elements of seq if it was sorted using f"
  (firstn n (sort seq f)))

(def sort (comparer sequence &optional (key #'identity))
  "Non-destructively sorts the sequence using comparer"
  (cl:sort sequence comparer :key key))
