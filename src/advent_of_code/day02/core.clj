(ns advent-of-code.day02.core
  (:require [clojure.string :as str]))

(defn find-area [dimensions]
  (let [dims (str/split dimensions #"x")
        [l w h] (mapv #(Integer/parseInt %) dims)
        areas [(* l w) (* w h) (* l h)]
        area-sum (reduce + areas)
        smallest (apply min areas)]
    (+ (* 2 area-sum) smallest)))

(defn run []
  (let [input (slurp "src/advent_of_code/day02/input.txt")
        lines (str/split input #"\n")
        area (reduce (fn [acc l]
                      (+ acc (find-area l))) 0 lines)]
    (println "Day 02:" area)))
