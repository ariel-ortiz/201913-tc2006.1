(require '[clojure.core.logic :as logic])
(require '[clojure.core.logic.fd :as fd])

(logic/defne sumo
  "Logical function that succeeds if the sum of all
  elements in lst is equal to result."
  [lst result]
  ([[] 0])
  ([[head . tail] result]
   (logic/fresh [temp]
     (sumo tail temp)
     (fd/+ head temp result))))

(logic/defne largesto
  "Logical function that succeeds if the largest element
  is lst is equal to result."
  [lst result]
  ([[x] x])
  ([[head . tail] head]
   (logic/fresh [temp]
     (largesto tail temp)
     (fd/> head temp)))
  ([[head . tail] temp]
   (largesto tail temp)
   (fd/>= temp head)))

(defn magic-square
  "Solves the 3x3 magic square puzzle."
  []
  (logic/run*
    [q1 q2 q3
     q4 q5 q6
     q7 q8 q9]
    (fd/in q1 q2 q3 q4 q5 q6 q7 q8 q9 (fd/interval 1 9))
    (fd/distinct [q1 q2 q3 q4 q5 q6 q7 q8 q9])
    (sumo [q1 q2 q3] 15)
    (sumo [q4 q5 q6] 15)
    (sumo [q7 q8 q9] 15)
    (sumo [q1 q4 q7] 15)
    (sumo [q2 q5 q8] 15)
    (sumo [q3 q6 q9] 15)
    (sumo [q1 q5 q9] 15)
    (sumo [q7 q5 q3] 15)))
