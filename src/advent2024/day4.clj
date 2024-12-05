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
  [input]
  (->> input
       (string/split-lines)
       (mapv vec)))


(defn- left-mas?
  [grid x y]
  (let [row (get grid y)]
    (if (< x 3) ; [S A _]
      false
      (= [\S \A \M] (subvec row (- x 3) x)))))

(left-mas? (parse-input example)
           4 1)
;; => true


(defn- right-mas?
  [grid x y]
  (let [row (get grid y)
        x-max (dec (count row))]
    (if (< (- x-max x) 4) ; [_ A S]
      false
      (= [\M \A \S] (subvec row (inc x) (+ x 4))))))

(right-mas? (parse-input example)
            5 0)
;; => true



(defn- transpose
  [grid]
  (apply mapv vector grid))

(parse-input example)
;; => [[\M \M \M \S \X \X \M \A \S \M]
;;     [\M \S \A \M \X \M \S \M \S \A]
;;     [\A \M \X \S \X \M \A \A \M \M]
;;     [\M \S \A \M \A \S \M \S \M \X]
;;     [\X \M \A \S \A \M \X \A \M \M]
;;     [\X \X \A \M \M \X \X \A \M \A]
;;     [\S \M \S \M \S \A \S \X \S \S]
;;     [\S \A \X \A \M \A \S \A \A \A]
;;     [\M \A \M \M \M \X \M \M \M \M]
;;     [\M \X \M \X \A \X \M \A \S \X]]

(transpose (parse-input example))
;; => [[\M \M \A \M \X \X \S \S \M \M]
;;     [\M \S \M \S \M \X \M \A \A \X]
;;     [\M \A \X \A \A \A \S \X \M \M]
;;     [\S \M \S \M \S \M \M \A \M \X]
;;     [\X \X \X \A \A \M \S \M \M \A]
;;     [\X \M \M \S \M \X \A \A \X \X]
;;     [\M \S \A \M \X \X \S \S \M \M]
;;     [\A \M \A \S \A \A \X \A \M \A]
;;     [\S \S \M \M \M \M \S \A \M \S]
;;     [\M \A \M \X \M \A \S \A \M \X]]


(defn- down-mas?
  [grid x y]
  (let [grid' (transpose grid)]
    (right-mas? grid' y x)))

(down-mas? (parse-input example) 9 3)
;; => true


(defn- up-mas?
  [grid x y]
  (let [grid' (transpose grid)]
    (left-mas? grid' y x)))

(up-mas? (parse-input example) 9 9)
;; => true


(defn- down-right-mas?
  [grid x y]
  (let [y-max (count grid)
        x-max (count (first grid))]
    (if (or (< (- x-max x) 4)
            (< (- y-max y) 4))
      false
      (let [m (get-in grid [(+ y 1) (+ x 1)])
            a (get-in grid [(+ y 2) (+ x 2)])
            s (get-in grid [(+ y 3) (+ x 3)])]
        (and (= \M m) (= \A a) (= \S s))))))

(down-right-mas? (parse-input example) 4 0)
;; => true


(defn- flip-horizontal
  [grid]
  (vec (reverse grid)))


(defn- flip-xy
  [n max]
  ;; given max = 9..
  ;; 0 => 9
  ;; 9 => 0
  ;; 1 => 8
  ;; 8 => 1
  ;; 3 => 6
  (get (vec (reverse (range 0 (inc max)))) n))


(flip-xy 0 9)
;; => 9
(flip-xy 9 9)
;; => 0
(flip-xy 8 9)
;; => 1
(flip-xy 1 9)
;; => 8
(flip-xy 3 9)
;; => 6

(defn- down-left-mas?
  [grid x y]
  (let [y-max (count grid)]
    (if (or (< x 3)
            (< (- y-max y) 4))
      false
      (let [m (get-in grid [(+ y 1) (- x 1)])
            a (get-in grid [(+ y 2) (- x 2)])
            s (get-in grid [(+ y 3) (- x 3)])]
        (and (= \M m) (= \A a) (= \S s))))))

(down-left-mas? (parse-input example) 9 3)
;; => true


(defn- up-right-mas?
  [grid x y]
  (down-right-mas?
   (flip-horizontal grid)
   x
   (flip-xy y (dec (count grid)))))

(up-right-mas? (parse-input example) 1 9)
;; => true


(defn- up-left-mas?
  [grid x y]
  (let [y' (flip-xy y (dec (count grid)))]
    (down-left-mas?
     (flip-horizontal grid)
     x
     y')))

(up-left-mas? (parse-input example) 3 9)
;; => true


(defn- debug
  [grid x y]
  (cond-> []
    (left-mas? grid x y) (conj :left)
    (right-mas? grid x y) (conj :right)
    (up-mas? grid x y) (conj :up)
    (down-mas? grid x y) (conj :down)
    (up-left-mas? grid x y) (conj :up-left)
    (up-right-mas? grid x y) (conj :up-right)
    (down-left-mas? grid x y) (conj :down-left)
    (down-right-mas? grid x y) (conj :down-right)))


(debug (parse-input example) 0 4)
;; => [:right]

(defn- part1
  [grid]
  (let [find-lr (fn [grid]
                  (apply + (for [x (range 0 (count (first grid)))
                                 y (range 0 (count grid))
                                 :when (= \X (get-in grid [y x]))]
                             (cond-> 0
                               (left-mas? grid x y) inc
                               (right-mas? grid x y) inc))))
        find-diag (fn [grid]
                    (apply + (for [x (range 0 (count (first grid)))
                                   y (range 0 (count grid))
                                   :when (= \X (get-in grid [y x]))]
                               (cond-> 0
                                 (down-left-mas? grid x y) inc
                                 (down-right-mas? grid x y) inc))))]
    (+ (find-lr grid)
       (find-diag grid)
       (find-lr (transpose grid)) ; up / down
       (find-diag (flip-horizontal grid)) ; up-left / down-left
       )))


(part1 (parse-input "..X...
.SAMX.
.A..A.
XMAS.S
.X...."))
;; => 4

(part1 (parse-input example))
;; => 18


(part1 (parse-input (slurp (io/resource "day4"))))
;; => 2582



(defn- xmas?
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
  [grid]
  (apply +
         (for [x (range 0 (count (first grid)))
               y (range 0 (count grid))
               :when (= \X (get-in grid [y x]))
               dir [[1 0] ;left
                    [-1 0] ;right
                    [0 -1] ;up
                    [0 1] ;down
                    [1 1] ;down-left
                    [-1 1] ;down-right
                    [1 -1] ;up-left
                    [-1 -1] ;up-right
                    ]
               :when (xmas? grid x y dir)]
           1)))


(part1 (parse-input example))
;; => 18

(part1 (parse-input (slurp (io/resource "day4"))))
;; => 2583



(defn x-mas?
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
  [grid]
  (apply + (for [x (range 0 (count (first grid)))
                 y (range 0 (count grid))
                 :when (x-mas? grid x y)]
             1)))


(part2 (parse-input example))
;; => 9


(part2 (parse-input (slurp (io/resource "day4"))))
;; => 1978
