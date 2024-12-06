(ns advent2024.day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))


(def example
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")


(defn- parse-input
  "Converts text grid into vector-of-vector of chars"
  [input]
  (->> input
       (string/split-lines)
       (mapv vec)))


(defn- xmas?
  "Takes a grid (vector-of-vectors), an x and y coordinate, and a direction
  vector, and returns true if the coordinates starting at x,y spell XMAS in the
  given direction."
  [grid x y dir]
  (loop [s (seq "XMAS")
         x x
         y y]
    (if-let [c1 (first s)]
      (let [c2 (get-in grid [y x])]
        (if (= c1 c2)
          (recur (rest s)
                 (+ x (first dir))
                 (+ y (second dir)))
          false))
      true)))


(xmas? (parse-input example)
       5 0 [1 0]) ; left
;; => true

(xmas? (parse-input example)
       4 1 [-1 0]) ; right
;; => true


(xmas? (parse-input example)
       9 9 [0 -1]) ; up
;; => true

(xmas? (parse-input example)
       9 3 [0 1]) ; down
;; => true


(xmas? (parse-input example)
       9 3 [-1 1]) ; down-left
;; => true

(xmas? (parse-input example)
       4 0 [1 1]) ; down-right
;; => true


(defn part1
  "Given a grid, counts all the times that XMAS is spelled frontwards,
  backwards, up, down, and diagonally."
  [grid]
  (apply + (for [x (range 0 (count (first grid)))
                 y (range 0 (count grid))
                 :when (= \X (get-in grid [y x]))
                 dir [[-1 0] ;left
                      [1 0] ;right
                      [0 -1] ;up
                      [0 1] ;down
                      [-1 1] ;down-left
                      [1 1] ;down-right
                      [-1 -1] ;up-left
                      [1 -1] ;up-right
                      ]
                 :when (xmas? grid x y dir)]
             1)))


(part1 (parse-input example))
;; => 18

(part1 (parse-input (slurp (io/resource "day4"))))
;; => 2583


(defn x-mas?
  "Given a grid and x y coordinates, returns true if it's an X-MAS, e.g.

  M   S
    A
  M   S"
  [grid x y]
  (let [m (get-in grid [y x])]
    (if (= \A m)
      (let [ul (get-in grid [(dec y) (inc x)])
            ur (get-in grid [(dec y) (dec x)])
            dl (get-in grid [(inc y) (inc x)])
            dr (get-in grid [(inc y) (dec x)])]
        (and (or (= [\M \S] [ul dr])
                 (= [\S \M] [ul dr]))
             (or (= [\M \S] [ur dl])
                 (= [\S \M] [ur dl]))))
      false)))


(defn part2
  "Counts the number of X-MAS's in the grid."
  [grid]
  (apply + (for [x (range 0 (count (first grid)))
                 y (range 0 (count grid))
                 :when (x-mas? grid x y)]
             1)))


(part2 (parse-input example))
;; => 9


(part2 (parse-input (slurp (io/resource "day4"))))
;; => 1978
