(in-package :experimental)

(defmacro def (name args &body body)
  "Same as clamp:def but allows ?, !, ssyntax, and argument 
   destrucuring."
  `(w/ssyntax
    ;; The arguments list has to be expanded manually. Otherwise
    ;; symbols such as ! would be considered ssyntax. The alternative
    ;; would be to put a couple of "patches" into the implementation of
    ;; ssyntax which would ignore those cases.
     ,(mvb (new-args alist) (parse-args args)
        (if (null alist)
            `(defun ,name ,new-args ,@body)
            `(defun ,name ,new-args
               (let ,(map #'cadr alist) (list ,@(map #'car alist))
                 ,@body))))))

(defmacro defmemo (name args &body body)
  "Same as clamp:defmemo but allows ?, !, ssyntax, and argument 
   destrucuring."
  `(w/ssyntax
    ;; The arguments list has to be expanded manually. Otherwise
    ;; symbols such as ! would be considered ssyntax. The alternative
    ;; would be to put a couple of "patches" into the implementation of
    ;; ssyntax which would ignore those cases.
     ,(mvb (new-args alist) (parse-args args)
        (if (null alist)
            `(clamp:defmemo ,name ,new-args ,@body)
            `(clamp:defmemo ,name ,new-args
               (let ,(map #'cadr alist) (list ,@(map #'car alist))
                 ,@body))))))

(defmacro mac (name args &body body)
  "Same as clamp:mac but allows ?, !, ssyntax, and argument 
   destrucuring."
  `(w/ssyntax
    ;; The arguments list has to be expanded manually. Otherwise
    ;; symbols such as ! would be considered ssyntax. The alternative
    ;; would be to put a couple of "patches" into the implementation of
    ;; ssyntax which would ignore those cases.
     ,(mvb (new-args alist) (parse-args args)
        (if (null alist)
            `(defmacro ,name ,new-args ,@body)
            `(defmacro ,name ,new-args
               (let ,(map #'cadr alist) (list ,@(map #'car alist))
                 ,@body))))))
