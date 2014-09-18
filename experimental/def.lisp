(in-package :experimental)

(mac def (name args &body body)
  "Same as clamp:defun but allows ?, !, and argument destrucuring."
  `(w/ssyntax
     ,(mvb (new-args alist) (parse-args args)
        (if (null alist)
            `(defun ,name ,new-args ,@body)
            `(defun ,name ,new-args
               (dbind ,(map #'cadr alist) (list ,@(map #'car alist))
                 ,@body))))))
