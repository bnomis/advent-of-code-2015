(ns advent-of-code.core
  (:gen-class)
  (:require
    [advent-of-code.day01.core]
    [advent-of-code.day02.core]
    [advent-of-code.day03.core]
    [advent-of-code.day04.core]))

(defn -main
  [& args]
  (advent-of-code.day01.core/run)
  (advent-of-code.day02.core/run)
  (advent-of-code.day03.core/run)
  (advent-of-code.day04.core/run))
