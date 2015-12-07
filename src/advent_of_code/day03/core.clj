(ns advent-of-code.day03.core)

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
      (count houses)
      (recur (move last-house m) (conj houses (move last-house m)) (first moves) (rest moves)))))

(defn run []
  (let [input (seq (slurp "src/advent_of_code/day03/input.txt"))
        count (find-houses input)]
    (println "Day 03:" count)))
