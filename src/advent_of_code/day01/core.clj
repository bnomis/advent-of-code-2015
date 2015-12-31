(ns advent-of-code.day01.core)

(defn find-floor [input]
  (let [count (reduce (fn [acc i]
                        (if (= \( i)
                          (inc acc)
                          (dec acc))) 0 input)]
    count))

(defn verticalise [floor action]
  (if (= \( action)
    (inc floor)
    (dec floor)))

(defn find-basement [input]
  (loop [floor 0
          pos 0
          current (first input)
          input (rest input)]
    (if (= -1 floor)
      pos
      (recur (verticalise floor current) (inc pos) (first input) (rest input)))))

(defn run []
  (let [input (seq (slurp "src/advent_of_code/day01/input.txt"))
        count (find-floor input)
        basement (find-basement input)]
    (println "Day 01, part 1:" count)
    (println "Day 01, part 2:" basement)))
