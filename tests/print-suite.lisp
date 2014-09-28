(in-package :clamp-tests)

(defsuite print (clamp))

(deftest pr (print)
  (assert-equal "hello world 5"
      (tostring (pr "hello" " world " (+ 2 3))))
  ;; This use of tostring is just so the output is not visible.
  (tostring (assert-eql 3 (pr (+ 1 2) (+ 4 5)))))

(deftest prn (print)
  (assert-equal (format nil "~%")
                (tostring (prn)))
  (assert-equal (format nil "Hello World 5~%")
                (tostring (prn "Hello" " World " (+ 3 2))))
  ;; This use of tostring is just so the output is not visible.
  (tostring (assert-eql 5 (prn (+ 1 4) (+ 3 7)))))

(deftest w/outstring (print)
  (assert-equal "Hello World 3" (w/outstring stream
				  (princ "Hello " stream)
				  (princ "World " stream)
				  (princ (+ 1 2) stream)))
  (assert-equal "" (w/outstring stream)))

(deftest tostring (print)
  (assert-equal "Hello World 3" (tostring (pr "Hello " "World " (+ 1 2))))
  (assert-equal "" (tostring))
  (assert-equal (format nil "~%") (tostring (prn))))

(deftest w/instring (print)
  (assert-eq 'hello (w/instring stream "Hello World" (read :from stream)))
  (assert-equal "Hello World" (w/instring stream "Hello World" (read-line :from stream)))
  (assert-equal 123 (w/instring stream "123" (parse-integer (read-line :from stream)))))

(deftest fromstring (print)
  (assert-eq 'hello (fromstring "Hello World" (read)))
  (assert-equal "Hello World" (fromstring "Hello World" (read-line)))
  (assert-eql 123 (fromstring "123" (parse-integer (read-line)))))
