;;;; Utilities for writing macros.

(in-package :clamp)

(mac w/uniq (names &body body)
  "Binds every symbol in NAMES to a uniq symbol. Then executes BODY."
  (if (consp names)
      `(with ,(mappend (fn (n) `(,n (uniq (symbol-name ',n))))
                       names)
             ,@body)
      `(let ,names (uniq (symbol-name ',names)) ,@body)))

(def mkstr (&rest args)
  "Returns the string representing all of the arguments."
  (tostring
    (apply #'pr args)))

(def symb (&rest args)
  "Returns a symbol representing all of the arguments."
  (values (intern (apply #'mkstr args))))

;; Based on arg-count in PAIP.
(def check-len (name form xs min &key (max nil)
                     (str "Wrong number of arguments for ~A in ~A: ~
                           ~A supplied, ~A~@[ to ~A~] expected."))
  "Asserts that some list, XS, has between MIN and MAX elements. If
   XS does not have the right number of arguments STR is the error
   string with NAME, FORM, (len XS), MIN, MAX as its arguments."
  (let len-xs (len xs)
    (assert (and (<= min len-xs (or max min)))
            () str name form len-xs min max)))

;; The macro once-only is from Practical Common Lisp. The license is
;; included because it is required for the license.

;; Copyright (c) 2005, Peter Seibel All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.

;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.

;;     * Neither the name of the Peter Seibel nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym (string n)))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
