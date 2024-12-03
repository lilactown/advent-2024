(ns advent2024.day3
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))


(def example
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn part1
  "Find all instructions which match mul(x,y) where x and y are numbers with 1-3
  digits. Return the sum of all x and y multiplied."
  [input]
  (->> input
       (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
       (map (comp #(map Integer/parseInt %)
                  rest)) ; drop instruction at the front of the match group
       (reduce (fn [acc [x y]]
                 (+ acc (* x y)))
               0)))


(part1 example)
;; => 161


(part1 (slurp (io/resource "day3")))
;; => 156388521


(def example2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn part2
  "Like part1, but disable all mul(x,y) that come after a don't(), enabling
  again after a do() instruction. Return the sum of all x and y multiplied for
  the enabled mul instructions."
  [input]
  (loop [acc 0
         enabled? true
         tape (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" input)]
    (let [[[token a b]] tape]
      (cond
        (nil? token) acc

        (string/starts-with? token "mul")
        (recur (if enabled?
                 (+ acc (* (Integer/parseInt a)
                           (Integer/parseInt b)))
                 acc)
               enabled?
               (rest tape))

        (= "don't()" token)
        (recur acc false (rest tape))

        (= "do()" token)
        (recur acc true (rest tape))))))


(part2 example2)
;; => 48


(part2 (slurp (io/resource "day3")))
;; => 75920122
