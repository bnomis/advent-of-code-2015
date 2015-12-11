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
    [advent-of-code.day09.core]))


(defn run-all []
  (advent-of-code.day01.core/run)
  (advent-of-code.day02.core/run)
  (advent-of-code.day03.core/run)
  (advent-of-code.day04.core/run)
  (advent-of-code.day05.core/run)
  (advent-of-code.day06.core/run)
  (advent-of-code.day07.core/run)
  (advent-of-code.day08.core/run)
  (advent-of-code.day09.core/run))

(defn run-day [day]
  (let [day (Integer/parseInt day)
        fn (format "(advent-of-code.day%02d.core/run)" day)]
    (eval (read-string fn))))

(defn -main
  [& args]
  (if (empty? args)
    (run-all)
    (do
      (mapv #(run-day %) args))))
