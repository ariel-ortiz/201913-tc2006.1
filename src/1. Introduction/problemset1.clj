; Some code to solve some of the problems in
; problem set #1.

(require '[clojure.test :refer [deftest is run-tests]])

(defn !
  "Returns the factorial of n."
  [n]
  (reduce *' (range 1 (inc n))))

(defn duplicate
  "Returns a new list with each element of lst duplicated."
  [lst]
  (mapcat #(list % %) lst))

(defn pow
  "Raises a to the b-th power."
  [a b]
  (reduce *' (repeat b a)))

(defn fib
  "Returns the n-th element of the Fibonacci sequence."
  [n]
  (loop [a 0
         b 1
         i 0]
    (if (= i n)
      a
      (recur b
             (+ a b)
             (inc i)))))

(deftest test-!
  (is (= 1
         (! 0)))
  (is (= 120
         (! 5)))
  (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
         (map ! (range 11))))
  (is (= 15511210043330985984000000N
         (! 25)))
  (is (= 815915283247897734345611269596115894272000000000N
         (! 40))))

(deftest test-duplicate
  (is (= '(1 1 2 2 3 3 4 4 5 5)
         (duplicate '(1 2 3 4 5))))
  (is (= ()
         (duplicate ())))
  (is (= '(a a)
         (duplicate '(a))))
  (is (= '(a a b b c c d d e e f f g g h h)
         (duplicate '(a b c d e f g h)))))

(deftest test-pow
  (is (= 1 (pow 0 0)))
  (is (= 0 (pow 0 1)))
  (is (= 1 (pow 5 0)))
  (is (= 5 (pow 5 1)))
  (is (= 125 (pow 5 3)))
  (is (= 25 (pow -5 2)))
  (is (= -125 (pow -5 3)))
  (is (= 1024 (pow 2 10)))
  (is (= 525.21875 (pow 3.5 5)))
  (is (= 129746337890625 (pow 15 12)))
  (is (= 3909821048582988049 (pow 7 22))))

(deftest test-fib
  (is (= 0
         (fib 0)))
  (is (= 1
         (fib 1)))
  (is (= 1
         (fib 2)))
  (is (= 5
         (fib 5)))
  (is (= '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
            987 1597 2584 4181 6765)
         (map fib (range 21))))
  (is (= 267914296
         (fib 42))))

(run-tests)