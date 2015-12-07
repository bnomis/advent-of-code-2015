(ns advent-of-code.day01.core)

(defn find-floor [input]
  ;;(println input)
  (let [count (reduce (fn [acc i]
                        (if (= \( i)
                          (inc acc)
                          (dec acc))) 0 input)]
    count))

(defn run []
  (let [input (seq (slurp "src/advent_of_code/day01/input.txt"))
        count (find-floor input)]
    (println "Day 01:" count)))
