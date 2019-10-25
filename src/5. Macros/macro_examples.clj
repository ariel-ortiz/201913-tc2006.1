(defmacro my-and
  ([] true)
  ([x] x)
  ([x & exprs]
   `(let [t# ~x]
       (if t#
         (my-and ~@exprs)
         t#))))
