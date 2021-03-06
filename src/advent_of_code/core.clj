(ns advent-of-code.core
  (:gen-class)
  (:require
    [advent-of-code.day01.core]
    [advent-of-code.day02.core]
    [advent-of-code.day03.core]
    [advent-of-code.day04.core]
    [advent-of-code.day05.core]
    [advent-of-code.day06.core]
    [advent-of-code.day07.core]
    [advent-of-code.day08.core]
    [advent-of-code.day09.core]
    [advent-of-code.day10.core]
    [advent-of-code.day11.core]
    [advent-of-code.day12.core]
    [advent-of-code.day13.core]
    [advent-of-code.day14.core]
    [advent-of-code.day15.core]
    [advent-of-code.day16.core]
    [advent-of-code.day17.core]
    [advent-of-code.day18.core]
    [advent-of-code.day19.core]
    [advent-of-code.day20.core]
    [advent-of-code.day21.core]
    [advent-of-code.day22.core]
    [advent-of-code.day23.core]
    [advent-of-code.day24.core]
    [advent-of-code.day25.core]))

(defn run-all []
  (advent-of-code.day01.core/run)
  (advent-of-code.day02.core/run)
  (advent-of-code.day03.core/run)
  (advent-of-code.day04.core/run)
  (advent-of-code.day05.core/run)
  (advent-of-code.day06.core/run)
  (advent-of-code.day07.core/run)
  (advent-of-code.day08.core/run)
  (advent-of-code.day09.core/run)
  (advent-of-code.day10.core/run)
  (advent-of-code.day11.core/run)
  (advent-of-code.day12.core/run)
  (advent-of-code.day13.core/run)
  (advent-of-code.day14.core/run)
  (advent-of-code.day15.core/run)
  (advent-of-code.day16.core/run)
  (advent-of-code.day17.core/run)
  (advent-of-code.day18.core/run)
  (advent-of-code.day19.core/run)
  (advent-of-code.day20.core/run)
  (advent-of-code.day21.core/run)
  (advent-of-code.day22.core/run)
  (advent-of-code.day23.core/run)
  (advent-of-code.day24.core/run)
  (advent-of-code.day25.core/run))

(defn run-day [day]
  (let [day (Integer/parseInt day)
        fmt (if (< day 10) "(advent-of-code.day%02d.core/run)" "(advent-of-code.day%d.core/run)")
        fn (format fmt day)]
    (eval (read-string fn))))

(defn -main
  [& args]
  (if (empty? args)
    (run-all)
    (mapv run-day args)))
