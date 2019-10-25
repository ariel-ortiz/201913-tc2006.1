;------------------------------------------------
; Solution.
;------------------------------------------------

(import 'clojure.lang.IFn)
(declare $eval)

;;---------------------------------------------------------
;; Defines a Closure type that implements the IFn
;; interface which provides instances of the type the
;; ability to be called using the apply function:
;;
;;     (def c (->Closure (atom {'a 5, '+ +}) '(x) '(+ a x)))
;;     => #'user/c
;;
;;     (apply c '(10))
;;     => 15
;;
(deftype Closure
  [env params body]

  IFn

  (applyTo [self args]
    ($eval body
      (merge @env (zipmap params args)))))

;;---------------------------------------------------------
(defn third
  "Return the third element of lst."
  [lst]
  (nth lst 2))

;;---------------------------------------------------------
(defn fourth
  "Return the fourth element of lst."
  [lst]
  (nth lst 3))

;;---------------------------------------------------------
(defn prtrue
  [x]
  (println x)
  true)

;;---------------------------------------------------------
(defn prfalse
  [x]
  (println x)
  false)

;;---------------------------------------------------------
(defn $eval
  "Evaluates expr using the bindings in the env map."
  [expr env]

  (cond

    ; Variable references
    (symbol? expr)
    (if (contains? env expr)
      (env expr)
      (throw (RuntimeException.
               (str "Unbound variable: " expr))))

    ; Check for special forms
    (list? expr)
    (case (first expr)

      nil
      ()

      quote
      (second expr)

      if
      (if ($eval (second expr) env)
        ($eval (third expr) env)
        ($eval (fourth expr) env))

      lambda
      (->Closure (atom env) (second expr) (third expr))

      label
      (let [closure ($eval (third expr) env)]
        (swap! (.env closure)
          #(assoc % (second expr) closure))
        closure)

      and
      (loop [exprs (rest expr)]
        (cond
          (empty? exprs)                  true
          (= 1 (count exprs))             ($eval (first exprs) env)
          (not ($eval (first exprs) env)) false
          :else                           (recur (rest exprs))))


      ; Ordinary function application
      (apply ($eval (first expr) env)
        (map #($eval % env) (rest expr))))

    ; Anything that is not a symbol or a list evals to itself
    :else
    expr))

;;; Don't modify the following lines.
(let [expr (read)
      env  (eval (read))]
  (print ($eval expr env)))