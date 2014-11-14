;;;; The definitions for different ssyntax.

(in-package :clamp-experimental)

(defssyntax-test notf (sym name)
  (declare (ignore sym))
  (and (len> name 1)
       (char= #\~ (char name 0))))

(defssyntax-sym-mac notf (sym name)
  `(,sym (notf #',(intern (cut name 1)))))

(defssyntax-macro notf (sym name)
  `(,sym (&body body)
     `(not (,',(intern (cut name 1)) ,@body))))

(defssyntax-test compose (sym name)
  (declare (ignore sym))
  (and (pos #\+ name)
       (len> name 2))) ; This removes + and 1+ from being detected.

(defssyntax-sym-mac compose (sym name)
  (ado (tokens name #\+)
       (map #'intern it)
       (map (fn (f) `#',f) it)
       `(,sym (compose ,@it))))

(defssyntax-macro compose (sym name)
  (ado (tokens name #\+)
       (map #'intern it)
       `(,sym (&body body)
          ;; (f+g+h ...) will expand into (f (g (h ...))) therefore
          ;; we need to work from the back and create a new list
          ;; containing the fn and the previous expression.
          (reduce #'list ',(butlast it)
                  :from-end t
                  :initial-value `(,',(last it) ,@body)))))

(defssyntax-test andf (sym name)
  (declare (ignore sym))
  (find #\& name))

(defssyntax-sym-mac andf (sym name)
  (ado (tokens name #\&)
       (map #'intern it)
       (map (fn (f) `#',f) it)
       `(,sym (andf ,@it))))

(defssyntax-macro andf (sym name)
  (declare (ignore name))
  `(,sym (&body body)
     `(call ,',sym ,@body)))

(defun get-ssyntax (c)
  "Is this ssyntax for get?"
  (in c #\. #\!))

(defssyntax-test get (sym name)
  (declare (ignore sym))
  (find #'get-ssyntax name))

(defssyntax-sym-mac get (sym name)
  (withs (ssyntaxes (keep #'get-ssyntax name)
          (obj . accessors) (map #'read-from-string
                                 (tokens name #'get-ssyntax))
          calls (map (fn (ss accessor)
                       (if (is ss #\.) accessor `',accessor))
                     ssyntaxes
                     accessors))
    `(,sym ,(reduce (fn (exp accessor)
                      `(get ,exp ,accessor))
                    calls
                    :initial-value obj))))
