(ns advent2024.day1
  (:require
   [advent2024.util :as u]
   [clojure.java.io :as io]))


(def example
  "3   4
4   3
2   5
1   3
3   9
3   3")


(defn parse-input
  [input]
  (let [nums (u/split-nums input)]
    [(take-nth 2 nums)
     (take-nth 2 (drop 1 nums))]))


(parse-input example)
;; => [(3 4 2 1 3 3) (4 3 5 3 9 3)]


(defn part1
  [[lefts rights]]
  ;; sort the two lists
  (let [lefts (sort lefts)
        rights (sort rights)]
    ;; find the distance
    (->> (map (fn [left right]
                (abs (- left right)))
              lefts rights)
         ;; add the distances
         (reduce + 0))))


(part1 (parse-input example))
;; => 11


(part1 (parse-input (slurp (io/resource "day1"))))
;; => 1590491


(defn part2
  [[lefts rights]]
  ;; get frequencies of right side
  (let [right-freqs (frequencies rights)]
    ;; multiply left numbers by frequency on right side to get similarity score
    (->> (map (fn [n]
                (* n (get right-freqs n 0)))
              lefts)
         ;; add up similarity scores
         (reduce + 0))))


(part2 (parse-input example))
;; => 31

(part2 (parse-input (slurp (io/resource "day1"))))
;; => 22588371
