;------------------------------------------------
; Solution.
;------------------------------------------------

(defn argswap
  "Takes a two argument function f and returns
  a new function that behaves like f but with
  its two arguments swapped."
  [f]
  (fn [x y]
    (f y x)))

;;; Don't modify the following lines.
(let [f (eval (read))
      x (read)
      y (read)]
  (print ((argswap f) x y)))