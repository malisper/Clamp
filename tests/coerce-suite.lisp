(in-package :clamp-experimental-tests)
(use-syntax :clamp)

(deftest table->list (clamp-experimental)
  ;; The results need to be sorted. Otherwise the order of the elements is undefined.
  (assert-equal '((1 2) (3 4) (5 6)) (sort #'< (coerce (obj 1 2 3 4 5 6) 'list) #'car))
  (assert-equal '((a 1) (b 2) (c 3)) (sort #'< (coerce (obj a 1 b 2 c 3) 'list) #'cadr)))

(deftest list->table (clamp-experimental)
  ;; The macro obj cannot be used here. Otherwise the tables would have different :tests.
  (assert-equalp (listtab '((1 2) (3 4) (5 6))) (coerce '((1 2) (3 4) (5 6)) 'hash-table))
  (assert-equalp (listtab '((a 1) (b 2) (c 3))) (coerce '((a 1) (b 2) (c 3)) 'hash-table)))
