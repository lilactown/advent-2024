(ns advent2024.day2
  (:require
   [advent2024.util :as u]
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def example
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")


(defn parse-input
  "Parse input into rows of numbers."
  [input]
  (->> (string/split-lines input)
       (map u/split-nums)))


(defn safe-level?
  "Given the delta between two numbers and direction, is it a \"safe\" input?"
  [delta direction]
  (and (= (pos? direction) (pos? delta))
       (not (zero? delta))
       (< 0 (abs delta) 4)))


(defn safe-report?
  "Given a row of numbers, are there any unsafe levels?"
  [report]
  (loop [levels report
         direction nil]
    (let [[x y] levels
          delta (- x (or y 0))]
      (cond
        (nil? y) true

        (safe-level? delta (or direction delta))
        (recur (rest levels) delta)))))


(safe-report? (last (parse-input example)))
;; => true


(defn part1
  "Count the number of reports (rows) which contain no unsafe levels."
  [reports]
  (count (filter safe-report? reports)))


(part1 (parse-input example))
;; => 2


(part1 (parse-input (slurp (io/resource "day2"))))
;; => 472




(defn safe-enough-report?
  "Given a row of numbers, is there at most one unsafe level?"
  [report]
  (if-not (safe-report? report)
    ;; there's at least one level causing it to be unsafe
    (loop [levels (vec report)
           n (dec (count report))] ; for each level in report
      (cond
        (neg? n) false ; we've checked all levels

        ;; see if report is safe without that level
        (safe-report? (into (subvec levels 0 n)
                            (subvec levels (inc n))))
        true

        ;; keep trying
        :else
        (recur (vec report) (dec n))))
    true))


(defn part2
  "Count number of reports (rows) which contain at most one unsafe level."
  [reports]
  (count (filter safe-enough-report? reports)))

(remove safe-enough-report? (parse-input example))


(safe-enough-report? '(8 6 4 4 1))

(part2 (parse-input example))
;; => 4


;; Checking edge cases

(remove safe-enough-report?
        (parse-input
         "48 46 47 49 51 54 56
1 1 2 3 4 5
1 2 3 4 5 5
5 1 2 3 4 5
1 4 3 2 1
1 6 7 8 9
1 2 3 4 3
9 8 7 6 7
7 10 8 10 11
29 28 27 25 26 25 22 20"))

(safe-enough-report? '(29 28 27 25 26 25 22 20))
(safe-enough-report? '(48 46 47 49 51 54 56))


(part2 (parse-input "48 46 47 49 51 54 56
1 1 2 3 4 5
1 2 3 4 5 5
5 1 2 3 4 5
1 4 3 2 1
1 6 7 8 9
1 2 3 4 3
9 8 7 6 7
7 10 8 10 11
29 28 27 25 26 25 22 20"))
;; => 10

(remove safe-enough-report?
        (parse-input "7 10 8 10 11
29 28 27 25 26 25 22 20"))

(safe-enough-report? '(7 10 8 10 11))

(safe-enough-report? '(9 8 7 7 7))

(safe-enough-report? '(29 28 27 25 26 25 22 20))


(part2 (parse-input (slurp (io/resource "day2"))))
;; => 520
