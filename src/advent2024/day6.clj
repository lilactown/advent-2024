(ns advent2024.day6
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))


(def example
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")


(defn parse-input
  [input]
  (->> (string/split-lines input)
       (mapv vec)))


(parse-input example)
;; => [[\. \. \. \. \# \. \. \. \. \.]
;;     [\. \. \. \. \. \. \. \. \. \#]
;;     [\. \. \. \. \. \. \. \. \. \.]
;;     [\. \. \# \. \. \. \. \. \. \.]
;;     [\. \. \. \. \. \. \. \# \. \.]
;;     [\. \. \. \. \. \. \. \. \. \.]
;;     [\. \# \. \. \^ \. \. \. \. \.]
;;     [\. \. \. \. \. \. \. \. \# \.]
;;     [\# \. \. \. \. \. \. \. \. \.]
;;     [\. \. \. \. \. \. \# \. \. \.]]



;;
;; I store all positions and directions as [y x] so that I can easily use get-in
;;

(defn start-pos
  [grid]
  (let [max-y (dec (count grid))]
    (loop [y 0]
      (let [x (->> (get grid y)
                   (keep-indexed
                    (fn [i ch]
                      (when (= \^ ch)
                        i)))
                   first)]
        (cond
          (some? x) [y x]
          (= max-y y) (throw (ex-info "Guard not found." {}))
          :else (recur (inc y)))))))


(start-pos (parse-input example))
;; => [4 6]


(def up [-1 0])
(def down [1 0])
(def left [0 -1])
(def right [0 1])


(defn move
  [pos dir]
  [(+ (nth pos 0) (nth dir 0))
   (+ (nth pos 1) (nth dir 1))])


(defn turn
  [dir]
  (case dir
    [-1 0] right ;up
    [1 0] left ;down
    [0 -1] up ;left
    [0 1] down ;right
    ))


(defn part1
  [grid]
  (let [pos (start-pos grid)]
    (loop [visited #{pos}
           pos pos
           dir up] ; up
      (let [next-pos (move pos dir)]
        (if-let [mark (get-in grid next-pos)]
          (case mark
            (\^ \.) (recur (conj visited next-pos) next-pos dir)
            \# (recur visited pos (turn dir)) ; don't move, turn
            )
          (count visited))))))


(part1 (parse-input example))
;; => 41

(part1 (parse-input (slurp (io/resource "day6"))))
;; => 5145
