;==========================================================
; Type your student ID and name here.
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

;==========================================================
(defn run-percentage
  "Solves problem 2 from the first practical exam."
  [v n]
  nil)

;==========================================================
(deftest test-run-percentage
  (is (= '(6 11 16 21 26 31 36 41 46)
         (run-percentage 3 17)))
  (is (= '(9 17 26 34 43 51 60 68 77)
         (run-percentage 5 17)))
  (is (= '(4 7 10 14 17 20 24 27 30)
         (run-percentage 3 11)))
  (is (= '(1 2 3 4 5 6 7 8 9)
         (run-percentage 1 10)))
  (is (= '(1 2 3 4 5 6 7 8 9)
         (run-percentage 3 3)))
  (is (= '(1 1 1 1 1 1 1 1 1)
         (run-percentage 1 1)))
  (is (= '(1 1 1 1 1 2 2 2 2)
         (run-percentage 1 2)))
  (is (= '(1000 2000 3000 4000 5000 6000 7000 8000 9000)
         (run-percentage 1 10000)))
  (is (= '(1000 1999 2998 3997 4996 5996 6995 7994 8993)
         (run-percentage 9992 1)))
  (is (= '(10000000 20000000 30000000 40000000 50000000
            60000000 70000000 80000000 90000000)
         (run-percentage 10000 10000))))

;==========================================================
(run-tests)
