;;;; Functions for specific kinds of sorting.

(in-package :clamp)

(def compare (comparer scorer)
  "Returns a function which compares its arguments score from SCORER
   using COMPARER. Generally should use the :key argument to other
   procedures instead of using compare."
  (fn (x y) (funcall comparer (funcall scorer x) (funcall scorer y))))

;;; It should be easy to modify best so that it works on all
;;; sequences as well as lists.
(def best (f xs &optional (key #'identity))
  "Finds the first element of the list XS if it was sorted using 
   the function F."
  (if (no xs)
      nil
      (ret wins (car xs)
        (let score (funcall key wins)
          (each elt (cdr xs)
            (let elt-score (funcall key elt)
              (when (funcall f elt-score score)
                (= wins elt
                   score elt-score))))))))

(def bestn (n f seq)
  "Returns a list containg the first N elements of SEQ if it was 
   sorted using the function F."
  (firstn n (sort f seq)))

(def nsort (comparer sequence &optional (key #'identity))
  "Destructively sorts SEQUENCE using COMPARER."
  (cl:sort sequence comparer :key key))

(def sort (comparer sequence &optional (key #'identity))
  "Non-destructively sorts SEQUENCE using COMPARER."
  (nsort comparer (copy-seq sequence) key))
