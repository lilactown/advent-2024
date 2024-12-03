(ns advent2024.util)


(defn split-nums
  [s]
  (->> (re-seq #"\d+" s)
       (map #(Integer/parseInt %))))
