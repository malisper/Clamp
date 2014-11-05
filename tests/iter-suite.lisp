(in-package :clamp-tests)

(defsuite iter (clamp))

(deftest rec (iter)
  (assert-eql 55
      (rec (n 10)
        (if (<= 0 n 1)
            n
            (+ (recur (- n 1))
               (recur (- n 2))))))
  (assert-eql 120
      (rec (n 5)
        (if (is n 0)
            1
            (* n (recur (- n 1))))))
  (assert-eql 720
      (rec (n 6 acc 1)
        (if (is n 0)
            acc
            (recur (- n 1) (* acc n))))))

(deftest repeat (iter)
  (assert-equal (n-of 10 5) (accum a (repeat 10 (a 5))))
  (assert-eql   1024 (ret result 1 (repeat 10 (zap #'* result 2))))
  (assert-eql   625  (ret result 5 (repeat 2 (zap [* _ _] result))))
  (assert-eql   55   (ret result 0
                       (let i 0
                         (repeat 10
                           (++ i)
                           (++ result i))))))

(deftest up (iter)
  (assert-equal (range 1 9) (accum a (up i 1 10 (a i))))
  (assert-eql   45 (ret result 0 (up i 1 10 (++ result i))))
  (assert-eql   20 (ret result 0 (up i 1 6 (++ result i) (++ result)))))

(deftest upto (iter)
  (assert-equal (range 1 10) (accum a (upto i 1 10 (a i))))
  (assert-eql   55 (ret result 0 (upto i 1 10 (++ result i))))
  (assert-eql   20 (ret result 0 (upto i 1 5 (++ result i) (++ result)))))

(deftest down (iter)
  (assert-equal (rev (range 1 9)) (accum a (down i 10 1 (a i))))
  (assert-eql  45 (ret result 0 (down i 10 1 (++ result i))))
  (assert-eql  20 (ret result 0 (down i 6 1 (++ result i) (++ result)))))

(deftest downto (iter)
  (assert-equal (rev (range 1 10)) (accum a (downto i 10 1 (a i))))
  (assert-eql   55 (ret result 0 (downto i 10 1 (++ result i))))
  (assert-eql   20 (ret result 0 (downto i 5 1 (++ result i) (++ result)))))

(deftest while (iter)
  (assert-equal '(t t t) (accum a
                            (fromstring "10 4 6 7 8"
                              (while (even (read))
                                (a t)))))
  (assert-eq 100 (ret result 0
                   (let counter 10
                     (while (> counter 0)
                       (++ result 10)
                       (-- counter 1))))))

(deftest until (iter)
  (assert-equal '(t t t) (accum a
                           (fromstring "10 4 6 7 8"
                             (until (odd (read))
                               (a t)))))
  (assert-eq 100 (ret result 0
                   (let counter 10
                     (until (<= counter 0)
                       (++ result 10)
                       (-- counter 1))))))

(deftest each (iter)
  (assert-equal (map [* _ _] (range 1 10))
                (accum a
                  (each x (range 1 10)
                    (a (* x x)))))
  (assert-eql 54
              (ret result 0
                (each x (range 1 9)
                  (++ result x)
                  (++ result)))))

(deftest on (iter)
  (assert-equal '((0 a) (1 b) (2 c))
                (accum a
                  (on x '(a b c)
                    (a (list index x)))))
  (assert-eql 110
              (ret result 0
                (on x (range 0 10)
                  (++ result x)
                  (++ result index)))))

(deftest whilet (iter)
  (assert-equal '(1 2 3) (accum a (fromstring "1 2 3"
                                    (whilet x (read :eof nil)
                                      (a x))))))

(deftest whiler (iter)
  (assert-equal '(1 2 3)
                (accum a (fromstring "1 2 3"
                  (whiler x (read :eof nil) nil
                    (a x)))))
  (assert-equal '(1 2 3)
                (accum a (fromstring "1 2 3"
                           (whiler x (read :eof t) t
                             (a x))))))

(deftest forlen (iter)
  (let xs '(1 2 3)
    (assert-equal (rev xs) (ret result '()
                             (forlen i xs
                               (push (elt xs i) result)))))
  (let seq #(1 2 3)
    (assert-equal (rev (coerce seq 'list))
                  (ret result '()
                    (forlen i seq
                      (push (elt seq i) result))))))
