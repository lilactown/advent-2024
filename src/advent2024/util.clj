(ns advent2024.util)


(defn split-nums
  [s]
  (->> (re-seq #"\d+" s)
       (map #(Long/parseLong %))))


(defmacro loop-br
  "Like loop but has a built in circuit breaker of 1,000."
  [bindings & body]
  `(let [itr# (volatile! 1000)]
     (loop ~bindings
       (when (zero? @itr#)
         (throw (ex-info "Run away loop"
                         ~(into {} (for [[binding _] (partition 2 bindings)]
                                     [(keyword binding) binding])))))
       (vswap! itr# dec)
       ~@body)))


#_(macroexpand
 '(loop-br [x 0]
   (recur (inc x))))
