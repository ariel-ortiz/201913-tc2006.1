; Some code to solve some of the problems in
; problem set #1.

(require '[clojure.test :refer [deftest is run-tests]])

(defn !
  "Returns the factorial of n."
  [n]
  (reduce *' (range 1 (inc n))))

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

(run-tests)