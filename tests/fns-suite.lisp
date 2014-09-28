(in-package :clamp-tests)

(defsuite fns (clamp))

(deftest rfn (fns)
  (let f (rfn fib (n)
           (if (<= 0 n 1)
               n
               (+ (fib (- n 1))
                  (fib (- n 2)))))
     (assert-eql 55 (call f 10))
     (assert-eql 34 (call f 9))))

(deftest afn (fns)
  (let f (afn (n)
           (if (<= 0 n 1)
               n
               (+ (self (- n 1))
                  (self (- n 2)))))
     (assert-eql 55 (call f 10))
     (assert-eql 34 (call f 9))))
