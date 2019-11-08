(require '[clojure.core.logic :as logic])

(logic/defne lasto
  "Logical function that succeeds if the last
  element of lst is x."
  [lst x]
  ([[x] x])
  ([[_ . t] x]
   (lasto t x)))

(logic/defne dupo
  "Logical function that succeeds if every
  element of lst is duplicated in the result."
  [lst result]
  ([[] []])
  ([[head . tail] [head head . temp]]
   (dupo tail temp)))

(logic/defne reverseo
  "Logical function that succeeds if the reverse
  of lst is result."
  [lst result]
  ([[] []])
  ([[head . tail] result]
   (logic/fresh [temp]
     (reverseo tail temp)
     (logic/appendo temp [head] result))))
