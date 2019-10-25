;------------------------------------------------
; Solution.
;------------------------------------------------

(defn maxfun
  "Returns the value of x where f(x) has its
  maximum value given that x is an integer and
  a <= x <= b."
  [f a b]
  (loop [x      a
         result x
         max    (f x)]
    (cond
      (> x b)       result
      (> (f x) max) (recur (inc x) x (f x))
      :else         (recur (inc x) result max))))

;;; Don't modify the following lines.
(let [f (eval (read))
      a (read)
      b (read)]
  (print (maxfun f a b)))