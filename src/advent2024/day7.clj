(ns advent2024.day7
  (:require
   [advent2024.util :as u]
   [clojure.java.io :as io]
   [clojure.string :as string]))


(def example
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")


(defn parse-input
  [input]
  (for [line (string/split-lines input)
        :let [[res & nums] (u/split-nums line)]]
    [res nums]))

(parse-input example)
;; => ([190 (10 19)]
;;     [3267 (81 40 27)]
;;     [83 (17 5)]
;;     [156 (15 6)]
;;     [7290 (6 8 6 15)]
;;     [161011 (16 10 13)]
;;     [192 (17 8 14)]
;;     [21037 (9 7 18 13)]
;;     [292 (11 6 16 20)])


(defn permutations
  [length options]
  (if (= 1 length)
    (map vector options)
    (for [tl (permutations (dec length) options)
          hd options]
      (list* hd tl))))

(permutations 2 '[+ *])
;; => ((+ *) (* *) (+ +) (* +))

(permutations 3 '[+ *])
;; => ((+ + *) (* + *) (+ * *) (* * *) (+ + +) (* + +) (+ * +) (* * +))

(permutations 4 '[+ *])
;; => ((+ + + *)
;;     (* + + *)
;;     (+ * + *)
;;     (* * + *)
;;     (+ + * *)
;;     (* + * *)
;;     (+ * * *)
;;     (* * * *)
;;     (+ + + +)
;;     (* + + +)
;;     (+ * + +)
;;     (* * + +)
;;     (+ + * +)
;;     (* + * +)
;;     (+ * * +)
;;     (* * * +))


(defn apply-eq
  [nums ops]
  (loop [acc (first nums)
         nums (rest nums)
         ops ops]
    (if-let [n (first nums)]
      (if-let [op (first ops)]
        (recur
         (case op
           + (+ acc n)
           * (* acc n)
           || (Long/parseLong (str acc n)))
         (rest nums)
         (rest ops))
        (throw (ex-info "Not enough ops" {})))
      acc)))

(apply-eq '[10 19] '[*])
;; => 190
(apply-eq '[10 19] '[+])
;; => 29
(apply-eq '[81 40 27] '[+ +])
;; => 148
(apply-eq '[81 40 27] '[* +])
;; => 3267
(apply-eq '[81 40 27] '[+ *])
;; => 3267
(apply-eq '[6 8 6 15] '[+ + +])
;; => 35
(apply-eq '[6 8 6 15] '[+ + *])
;; => 300
(apply-eq '[6 8 6 15] '[* + +])
;; => 69
(apply-eq [15 6] '[||])
;; => 156


(defn solvable?
  [equation options]
  (let [[res nums] equation]
    (loop [combos (permutations (dec (count nums)) options)]
      (if-let [ops (first combos)]
        (if (= res (apply-eq nums ops))
          true
          (recur (rest combos)))
        false))))

(solvable? [190 '(10 19)] '[+ *])
;; => true
(solvable? [3267 '(81 40 27)] '[+ *])
;; => true
(solvable? '[83 (17 5)] '[* +])
;; => false
(solvable? '[7290 (6 8 6 15)] '[* +])
;; => false

#_(time (count (permutations 12 '[+ *])))

#_(time (solvable? '[15644706 (5 7 8 1 7 9 3 6 621 3 9 6)]
                   '[+ *]))


(defn part1
  [equations]
  (->> (filter #(solvable? % '[+ *]) equations)
       (map first)
       (reduce + 0)))


(part1 (parse-input example))
;; => 3749

(part1 (parse-input (slurp (io/resource "day7"))))
;; => 21572148763543


(defn part2
  [equations]
  (->> (filter #(solvable? % '[+ * ||]) equations)
       (map first)
       (reduce + 0)))


(solvable? [156 [15 6]] '[+ * ||])
;; => true


(part2 (parse-input example))
;; => 11387


(part2 (parse-input (slurp (io/resource "day7"))))
;; => 581941094529163
