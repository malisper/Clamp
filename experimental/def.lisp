(in-package :experimental)

(mac def (name args &body body)
  "Same as clamp:def but allows ?, !, and argument destrucuring."
  `(w/ssyntax
    ;; The arguments list has to be expanded manually. Otherwise
    ;; symbols such as ! would be considered ssyntax. The alternative
    ;; would be to put a couple of "patches" into the implementation of
    ;; ssyntax which would ignore those cases.
     ,(mvb (new-args alist) (parse-args args)
        (if (null alist)
            `(defun ,name ,new-args ,@body)
            `(defun ,name ,new-args
               (dbind ,(map #'cadr alist) (list ,@(map #'car alist))
                 ,@body))))))
