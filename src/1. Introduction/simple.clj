; Simple Clojure exercises

(require '[clojure.test :refer [deftest is run-tests]])
(require '[clojure.math.numeric-tower :refer [sqrt]])

(defn f2c
  "Converts f degrees Fahrenheit to degrees Celsius."
  [f]
  (/ (* (- f 32)
        5)
     9))

(defn sign
  "Returns -1 if n is negative, 1 if n is positive
  greater than zero, or 0 if n is zero."
  [n]
  (if (< n 0)
    -1
    (if (> n 0)
      1
      0)))

(defn roots
  "Returns the solution to a quadratic equation given its
  three coefficients a, b, and c."
  [a b c]
  (let [d  (- b)
        e  (sqrt (- (* b b)
                    (* 4 a c)))
        f  (* 2 a)
        x1 (/ (+ d e) f)
        x2 (/ (- d e) f)]
    [x1 x2]))

(defn bmi
  "Returns the description of the body mass index given
  the weight (in kilograms) and height (in meters) of
  a person."
  [weight height]
  (let [BMI (/ weight (* height height))]
    (cond
      (< BMI 20) 'underweight
      (< BMI 25) 'normal
      (< BMI 30) 'obese1
      (< BMI 40) 'obese2
      :else      'obese3)))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(deftest test-bmi
  (is (= 'underweight (bmi 45 1.7)))
  (is (= 'normal (bmi 55 1.5)))
  (is (= 'obese1 (bmi 76 1.7)))
  (is (= 'obese2 (bmi 81 1.6)))
  (is (= 'obese3 (bmi 120 1.6))))

(run-tests)