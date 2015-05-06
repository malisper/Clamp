(in-package :clamp)
(use-syntax :clamp)

(defvar savers* (table) "A table containing the save functions.")

(mac fromdisk (var file init load save)
  "Define a variable which if unbound, its value will be taken from
   FILE if it exists. If it does not exist, it is found by calling
   LOAD on the file. The variable can be saved by calling todisk
   which will save it using SAVE."
  (w/uniq (gf gv)
    `(defvar ,var (do1 (iflet ,gf (file-exists ,file)
                              (call ,load ,gf)
                              ,init)
                    (= (gethash ',var savers*)
                       (fn (,gv)
                         (call ,save ,gv ,file)))))))

(mac diskvar (var file)
  "Creates a variable whose value will come from FILE."
  `(fromdisk ,var ,file nil #'readfile1 #'writefile))

(mac todisk (var &optional (expr var))
  "Saves the value of VAR according to its value in savers*."
  `(call (gethash ',var savers*)
         ,(if (is var expr)
              var
              `(= ,var ,expr))))
