(ns advent-of-code.day03.core
  (:require [clojure.set]))

(defn move [[x y] move]
  (case move
    \^ [x (+ y 1)]
    \v [x (- y 1)]
    \> [(+ x 1) y]
    \< [(- x 1) y]
    "error"))

(defn find-houses [moves]
  (loop [ last-house [0 0]
          houses #{last-house}
          m (first moves)
          moves (rest moves)]
    (if-not m
      houses
      (recur (move last-house m) (conj houses (move last-house m)) (first moves) (rest moves)))))

(defn filter-moves [moves start]
  (let [max (count moves)]
    (loop [ out []
            index start]
      (if (>= index max)
        out
        (recur (conj out (nth moves index)) (+ 2 index))))))

(defn find-houses-robo [moves]
  (let [santa (find-houses (filter-moves moves 0))
        robo  (find-houses (filter-moves moves 1))]
    (clojure.set/union santa robo)))

(defn run []
  (let [input (seq (slurp "src/advent_of_code/day03/input.txt"))
        santa (count (find-houses input))
        robo  (count (find-houses-robo input))]
    (println "Day 03, part 1:" santa)
    (println "Day 03, part 2:" robo)))
