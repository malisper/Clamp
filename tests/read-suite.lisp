(in-package :clamp-tests)
(use-syntax :clamp)

(defsuite read (clamp))

(deftest readc (read)
  (fromstring "hello"
    (assert-eql #\h (readc))
    (assert-eql #\e (readc))
    (assert-eql #\l (readc))
    (w/instring in "world"
      (assert-eql #\l (readc))
      (assert-eql #\w (readc :from in))
      (assert-eql #\o (readc :from in))
      (assert-eql #\o (readc))
      (assert-eql #\r (readc :from in))
      (assert-eql #\l (readc :from in))
      (assert-eql #\d (readc :from in)))
    (assert-eql '() (readc :eof nil))))

(deftest peekc (read)
  (fromstring "hello"
    (assert-eql #\h (peekc))
    (assert-eql #\h (peekc))
    (w/instring in "world"
      (assert-eql #\h (peekc))
      (assert-eql #\w (peekc :from in))
      (assert-eql #\w (peekc :from in)))))

(deftest read (read)
  (fromstring "(1 2 3 4)5"
    (assert-equal '(1 2 3 4) (read))
    (w/instring in "hello 6"
      (assert-eq 'hello (read :from in))
      (assert-eql 5 (read))
      (assert-eql 6 (read :from in))
      (assert-eql nil (read :eof nil)))))
