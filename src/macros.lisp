;;;; macros for writing macros

(in-package "CLAMP")

;;; Does not work because the substitution of the autouniqs only happens
;;; once when the macro is generated. Not each time the macro is executed
;;; like they should be.

;; (defmacro mac (name args &body body)
;;   "The same as defmacro except allows auto-uniq ie any symbol that ends with
;;    '@' will be replaced by a uniq symbol"
;;   (let uniqs (table)
;;     `(defmacro ,name ,args
;;        ,@(subst #'auto
;; 		[or (gethash _ uniqs) (= (gethash _ uniqs) (uniq (symbol-name _)))]
;; 		body))))

(mac w/uniq (names &body body)
  "Binds each element in names (or names if it is just a symbol), with
   a unique symbol"
  (if (consp names)
      `(with ,(mappend (fn (n) `(,n (uniq (symbol-name ',n))))
		       names)
	 ,@body)
      `(let ,names (uniq (symbol-name ',names)) ,@body)))
