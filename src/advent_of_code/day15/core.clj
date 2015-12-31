(ns advent-of-code.day15.core
  (:require
    [clojure.string :as str]
    [advent-of-code.day15.logic :refer [make-values]]))

;; data structures

;; ingredient
;; Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
;; {:name Butterscotch, :capacity -1, :durability -2, :flavor 6, :texture 3, :calories 8}

(defn score-for [ingredient quantity keys]
  (loop [out []
          k (first keys)
          keys (rest keys)]
    (if-not k
      out
      (recur (conj out (* quantity (get ingredient k))) (first keys) (rest keys)))))

(defn make-scores [ingredients values]
  (let [keys [:capacity :durability :flavor :texture]
        length (count ingredients)]
    (loop [scores []
            index 0]
      (if (>= index length)
        scores
        (recur (conj scores (score-for (nth ingredients index) (nth values index) keys)) (inc index))))))

(defn add-scores [scores index]
  (loop [vals []
          s (first scores)
          scores (rest scores)]
    (if-not s
      (let [t (reduce + vals)]
        (if (<= t 0)
          0
          t))
      (recur (conj vals (get s index)) (first scores) (rest scores)))))

(defn reduce-scores [scores]
  ;;(println "reduce-scores:" scores)
  (let [length (count (get scores 0))]
    (loop [sums []
            index 0]
      (if (>= index length)
        (reduce * sums)
        (recur (conj sums (add-scores scores index)) (inc index))))))

(defn score-add [out ingredients values]
  (let [scores (make-scores ingredients values)
        total (reduce-scores scores)]
    (if (zero? total)
      out
      (conj out total))))

(defn score [ingredients values]
  (loop [out []
          v (first values)
          values (rest values)]
    (if-not v
      out
      (recur (score-add out ingredients v) (first values) (rest values)))))

(defn calory-total [ingredients values]
  (let [length (count values)]
    (loop [vals []
            index 0]
      (if (>= index length)
        (reduce + vals)
        (recur (conj vals (* (:calories (get ingredients index)) (get values index))) (inc index))))))

(defn score-add-calories [out ingredients values calories]
  (if-not (= calories (calory-total ingredients values))
    out
    (let [scores (make-scores ingredients values)
          total (reduce-scores scores)]
      (if (zero? total)
        out
        (conj out total)))))

(defn score-calories [ingredients values calories]
  (loop [out []
          v (first values)
          values (rest values)]
    (if-not v
      out
      (recur (score-add-calories out ingredients v calories) (first values) (rest values)))))

(defn remove-last-char [s]
  (let [parts (vec (seq s))
        length (count parts)]
    (str/join (subvec parts 0 (- length 1)))))

(defn line->ingredient [line]
  (let [tokes (str/split line #" ")
        name (remove-last-char (get tokes 0))
        capacity (Integer/parseInt (remove-last-char (get tokes 2)))
        durability (Integer/parseInt (remove-last-char (get tokes 4)))
        flavor (Integer/parseInt (remove-last-char (get tokes 6)))
        texture (Integer/parseInt (remove-last-char (get tokes 8)))
        calories (Integer/parseInt (get tokes 10))]
    {:name name :capacity capacity :durability durability :flavor flavor :texture texture :calories calories}))

(defn lines->ingredients [lines]
  (mapv line->ingredient lines))

(defn read-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->ingredients lines)))

(defn max-score [file]
  (let [ingredients (read-file file)
        values (make-values (count ingredients))
        scores (score ingredients values)]
    (apply max scores)))

(defn max-score-calories [file calories]
  (let [ingredients (read-file file)
        values (make-values (count ingredients))
        scores (score-calories ingredients values calories)]
    (apply max scores)))

(defn run []
  (println "Day 15, part 1:" (max-score "src/advent_of_code/day15/input.txt"))
  (println "Day 15, part 2:" (max-score-calories "src/advent_of_code/day15/input.txt" 500)))
