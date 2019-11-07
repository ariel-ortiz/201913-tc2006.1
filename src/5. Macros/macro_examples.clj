(defmacro my-and
  "Our own implementation of the and macro."
  ([] true)
  ([x] x)
  ([x & exprs]
   `(let [t# ~x]
       (if t#
         (my-and ~@exprs)
         t#))))

(defn seek-delimited
  "Returns a sequence with the elements contained in args that are
  between start and end."
  [args start end]
  (->>
    args
    (drop-while #(not= % start))
    rest
    (take-while #(not= % end))))

(defmacro IF
  "Provide a conditional statement that is syntactically
  a bit more similar to those found in languages like
  Pascal or Fortran. It has the following form:

  (IF condition :THEN exp1 exp2 ... :ELSE exp3 exp4 ...)"
  [condition & args]
  `(if ~condition
     (do ~@(seek-delimited args :THEN :ELSE))
     (do ~@(seek-delimited args :ELSE :THEN))))
