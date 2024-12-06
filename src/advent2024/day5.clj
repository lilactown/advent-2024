(ns advent2024.day5
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]))


(def example
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")


(defn parse-input
  "Given input, returns a tuple where the first value is the table of rules and
  the second is the updates to be made.

  The table of rules is a hash map where the key is a page number and the value
  is a set of page numbers that should come after it."
  [input]
  (let [lines (string/split-lines input)
        rules (update-vals
               (->> lines
                    (take-while #(not= "" %))
                    (map #(->> (string/split % #"\|")
                               (map Integer/parseInt)))
                    (group-by first))
               (fn [v]
                 (set (map second v))))
        updates (->> lines
                     (drop-while #(not= "" %))
                     (rest)
                     (map #(->> (string/split % #",")
                                (mapv Integer/parseInt))))]
    [rules updates]))

(parse-input example)
;; => [{47 #{13 61 29 53},
;;      97 #{75 13 61 29 47 53},
;;      75 #{13 61 29 47 53},
;;      61 #{13 29 53},
;;      29 #{13},
;;      53 #{13 29}}
;;     ([75 47 61 53 29]
;;      [97 61 53 29 13]
;;      [75 29 13]
;;      [75 97 47 61 53]
;;      [61 13 29]
;;      [97 13 75 29 47])]


(defn valid-update?
  "Given an update (vector of numbers) and a table of rules, return an x"
  [u rules]
  (loop [u u
         seen #{}]
    (if-let [x (first u)]
      (if (seq (set/intersection seen (get rules x)))
        ;; we've already seen some numbers that should come after
        false
        (recur (rest u) (conj seen x)))
      true)))


(defn middle
  [v]
  (get v (quot (count v) 2)))


(defn part1
  [[rules updates]]
  (->> updates
       (filter #(valid-update? % rules))
       (map middle)
       (reduce + 0)))

(part1 (parse-input example))
;; => 143


(part1 (parse-input (slurp (io/resource "day5"))))
;; => 6260



(defn fix-update
  [u rules]
  (vec
   (sort
    (fn [a b]
      (cond
        (= a b) 0
        (contains? (get rules a) b) -1
        :else 1))
    u)))


(defn part2
  [[rules updates]]
  (->> updates
       (remove #(valid-update? % rules))
       (map #(fix-update % rules))
       (map middle)
       (reduce + 0)))


(part2 (parse-input example))
;; => 123


(part2 (parse-input (slurp (io/resource "day5"))))
;; => 5346
