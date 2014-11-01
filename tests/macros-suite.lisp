(in-package :clamp-tests)

(defsuite macros (clamp))

(deftest mkstr (macros)
  (assert-equalp "AbcDE5" (mkstr 'a "bc" 'de 5))
  (assert-equalp "AbcDE5" (mkstr #\A "bc" 'de "5")))

(deftest symb (macros)
  (assert-eq '|AbcDE5| (symb 'a "bc" 'de 5))
  (assert-eq '|AbcDE5| (symb #\A "bc" 'de "5")))
