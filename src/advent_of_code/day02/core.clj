(ns advent-of-code.day02.core
  (:require [clojure.string :as str]))

(defn find-area [dimensions]
  (let [dims (str/split dimensions #"x")
        [l w h] (mapv #(Integer/parseInt %) dims)
        areas [(* l w) (* w h) (* l h)]
        area-sum (reduce + areas)
        smallest (apply min areas)]
    (+ (* 2 area-sum) smallest)))

(defn find-ribbon [dimensions]
  (let [dims (str/split dimensions #"x")
        [l w h] (mapv #(Integer/parseInt %) dims)
        sorted (sort [l w h])
        first (first sorted)
        second (second sorted)
        length (* 2 (+ first second))
        volume (* l w h)]
    (+ volume length)))

(defn run []
  (let [input (slurp "src/advent_of_code/day02/input.txt")
        lines (str/split input #"\n")
        area (reduce (fn [acc l]
                      (+ acc (find-area l))) 0 lines)
        ribbon (reduce (fn [acc l]
                        (+ acc (find-ribbon l))) 0 lines)]
    (println "Day 02, part 1:" area)
    (println "Day 02, part 2:" ribbon)))
