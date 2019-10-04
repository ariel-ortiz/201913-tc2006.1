(declare variable?
         same-variable?
         sum?
         augend
         addend
         make-sum
         product?
         multiplicand
         multiplier
         make-product)

(defn deriv
  "Derives exp with respect to var"
  [exp var]
  (cond

    (number? exp)
    0

    (variable? exp)
    (if (same-variable? exp var)
      1
      0)

    (sum? exp)
    (make-sum (deriv (augend exp) var)
              (deriv (addend exp) var))

    (product? exp)
    (make-sum (make-product (multiplicand exp)
                            (deriv (multiplier exp) var))
              (make-product (multiplier exp)
                            (deriv (multiplicand exp) var)))))

(defn variable?
  "Is exp a variable?"
  [exp]
  (symbol? exp))

(defn same-variable?
  "Are v1 and v2 the same variable?"
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn sum?
  "Is exp a sum?"
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '+ (first exp))))

(defn augend
  "Return the augend of a sum."
  [exp]
  (nth exp 1))

(defn addend
  "Return the addend of a sum."
  [exp]
  (nth exp 2))

(defn make-sum
  "Construct the sum of a1 and a2"
  [a1 a2]
  (cond
    (= a1 0) a2
    (= a2 0) a1
    (and (number? a1)
         (number? a2)) (+ a1 a2)
    :else (list '+ a1 a2)))

(defn product?
  "Is exp a product?"
  [exp]
  (and (list? exp)
       (= 3 (count exp))
       (= '* (first exp))))

(defn multiplicand
  "Multiplicand of the product exp."
  [exp]
  (nth exp 1))

(defn multiplier
  "Multiplier of the product exp."
  [exp]
  (nth exp 2))

(defn make-product
  "Construct the product of m1 and m2."
  [m1 m2]
  (cond
    (= m1 0) 0
    (= m2 0) 0
    (= m1 1) m2
    (= m2 1) m1
    (and (number? m1)
         (number? m2)) (* m1 m2)
    :else (list '* m1 m2)))