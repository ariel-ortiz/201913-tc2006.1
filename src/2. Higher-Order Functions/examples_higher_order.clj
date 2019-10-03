(defn my-map
  "Returns a list resulting from applying fun to each
  element in lst."
  [fun lst]
  (if (empty? lst)
    ()
    (cons (fun (first lst))
          (my-map fun (rest lst)))))

(defn make-mul-fun
  "Makes a new multiply by x function."
  [x]
  (fn [y]
    (* x y)))

(defn fun
  [a b c d e]
  (* (+ a b) (/ c d) e))

(defn fun-curry
  [a]
  (fn [b]
    (fn [c]
      (fn [d]
        (fn [e]
          (* (+ a b) (/ c d) e))))))


(defn composite
  "Returns a composite function of f and g."
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (* x x))
(defn f2 [x] (+ x 3))
(def f3 (composite f1 f2))
(def f4 (composite f2 f1))
(def f5 (composite f4 f3))

(defn qsort
  [lst]
  (if (empty? lst)
    ()
    (let [less-list (filter #(< % (first lst)) (rest lst))
          greater-or-equal-list (filter #(>= % (first lst)) (rest lst))]
      (concat (qsort less-list)
              (list (first lst))
              (qsort greater-or-equal-list)))))