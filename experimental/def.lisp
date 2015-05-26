(in-package :experimental)
(use-syntax :clamp)

(defmacro defexperimental (new old)
  "Defines a definition special form, NEW, which is the same as the
   definition special form, OLD, except it allows for ssyntax,
   and the new argument lists."
  `(defmacro ,new (name args &body body)
     `(w/ssyntax
        ,(mvb (new-args alist) (parse-args args)
           (if (null alist)
               `(,',old ,name ,new-args ,@body)
               `(,',old ,name ,new-args
                  (let ,(map #'cadr alist) (list ,@(map #'car alist))
                    ,@body)))))))

(defexperimental def clamp:def)
(defexperimental defmemo clamp:defmemo)
(defexperimental mac clamp:mac)

(mac defmethod (name args &body body)
  "Version of defmethod that allows for ssyntax."
  `(w/ssyntax (cl:defmethod ,name ,args ,@body)))
