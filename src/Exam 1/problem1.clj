;==========================================================
; Solution.
;==========================================================

(require '[clojure.test :refer [deftest is run-tests]])

;==========================================================
(defn buffoon?
  "Solves problem 1 from the first practical exam."
  [lst]
  (>= (first lst)
      (apply max (rest lst))))

;==========================================================
(deftest test-buffoon?
  (is (buffoon? '(1000 1000 1000)))
  (is (not (buffoon? '(1 2 3 4 5))))
  (is (buffoon? '(5 4)))
  (is (not (buffoon? '(6 6 6 6 6 6 6 6 6 6
                        6 6 6 6 6 6 6 6 6 6
                        6 6 6 6 6 6 6 6 6 6
                        6 6 6 6 6 6 6 6 6 6
                        6 9 6 6 6 6 6 6 6 6
                        6 6 6 6 6 6 6 6 6 6))))
  (is (buffoon? '(42 8 23 4 16 15)))
  (is (not (buffoon? '(15 42 8 4 23 16))))
  (is (buffoon? '(99 50 79 39 50 51 58 95 72 4
                   57 17 40 86 65 1 98 38 20 77
                   18 82 23 91 9 4 25 62 26 57
                   42 73 63 5 76 10 97 2 97 12
                   61 81 93 19 1 82 2 23 46 28
                   36 59 91 43 79 71 97 35 83 19
                   50 78 41 44 13 82 92 88 65 39
                   50 77 36 59 46 76 79 54 65 39
                   26 21 94 53 6 56 70 82 57 98
                   59 42 71 49 89 17 85 96 80 27)))
  (is (not (buffoon? '(11 46 17 53 92 7 71 62 62 2
                        53 65 48 23 34 44 99 67 44 81
                        67 90 43 11 97 9 22 46 100 76
                        48 70 100 79 74 20 80 25 37 82
                        48 8 98 45 76 18 36 42 64 12
                        42 74 78 71 29 8 58 39 37 34
                        25 26 3 8 84 78 92 13 35 77
                        84 14 86 51 35 61 84 35 85 45
                        62 70 79 50 61 17 11 61 7 31
                        82 12 80 43 11 0 73 78 79 17)))))

;==========================================================
(run-tests)
