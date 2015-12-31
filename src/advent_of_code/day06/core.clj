(ns advent-of-code.day06.core
  (:require [clojure.string :as str]))

(defn make-row [width]
  (vec (repeat width 0)))

(defn make-grid [width height]
  (vec (repeatedly height #(make-row width))))

(defn turn-on [grid x y]
  (assoc-in grid [y x] 1))

(defn turn-off [grid x y]
  (assoc-in grid [y x] 0))

(defn toggle [grid x y]
  (let [current (get-in grid [y x])
        new (if (zero? current) 1 0)]
    (assoc-in grid [y x] new)))

(defn turn-on-2 [grid x y]
  (update-in grid [y x] inc))

(defn turn-off-2 [grid x y]
  (update-in grid [y x] (fn [v] (max 0 (dec v)))))

(defn toggle-2 [grid x y]
  (update-in grid [y x] #(+ 2 %)))

(defn row-sums [grid]
  (mapv (fn [g] (reduce + 0 g)) grid))

(defn count-grid [grid]
  (reduce + 0 (row-sums grid)))

(defn parse-coords [coords]
  (let [[x y] (str/split coords #",")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn parse-instruction [instruction]
  (let [parts (str/split instruction #" ")]
    (case (get parts 0)
      "toggle" {:operation :toggle :top-left (parse-coords (get parts 1)) :bottom-right (parse-coords (get parts 3))}
      "turn" (case (get parts 1)
              "on"  {:operation :turn-on  :top-left (parse-coords (get parts 2)) :bottom-right (parse-coords (get parts 4))}
              "off" {:operation :turn-off :top-left (parse-coords (get parts 2)) :bottom-right (parse-coords (get parts 4))}
              nil)
      nil)))

(defn do-operation [grid x y operation]
  (case operation
    :turn-on (turn-on grid x y)
    :turn-off (turn-off grid x y)
    :toggle (toggle grid x y)
    nil))

(defn process-columns [grid y tx bx operation]
  (loop [grid grid
          x tx]
    (if (> x bx)
      grid
      (recur (do-operation grid x y operation) (inc x)))))

(defn process-rows [grid tx ty bx by operation]
  (loop [grid grid
          y ty]
    (if (> y by)
      grid
      (recur (process-columns grid y tx bx operation) (inc y)))))

(defn process-instruction [grid instruction]
  (let [{:keys [:top-left :bottom-right :operation]} (parse-instruction instruction)
        [tx ty] top-left
        [bx by] bottom-right]
    (process-rows grid tx ty bx by operation)))

(defn run-instructions [width height instructions]
  (let [grid (make-grid width height)
        grid (reduce process-instruction grid instructions)]
    (count-grid grid)))


(defn do-operation-2 [grid x y operation]
  (case operation
    :turn-on (turn-on-2 grid x y)
    :turn-off (turn-off-2 grid x y)
    :toggle (toggle-2 grid x y)
    nil))

(defn process-columns-2 [grid y tx bx operation]
  (loop [grid grid
          x tx]
    (if (> x bx)
      grid
      (recur (do-operation-2 grid x y operation) (inc x)))))

(defn process-rows-2 [grid tx ty bx by operation]
  (loop [grid grid
          y ty]
    (if (> y by)
      grid
      (recur (process-columns-2 grid y tx bx operation) (inc y)))))

(defn process-instruction-2 [grid instruction]
  (let [{:keys [:top-left :bottom-right :operation]} (parse-instruction instruction)
        [tx ty] top-left
        [bx by] bottom-right]
    (process-rows-2 grid tx ty bx by operation)))

(defn run-instructions-2 [width height instructions]
  (let [grid (make-grid width height)
        grid (reduce process-instruction-2 grid instructions)]
    (count-grid grid)))

(defn run []
  (let [input (slurp "src/advent_of_code/day06/input.txt")
        lines (str/split input #"\n")
        count (run-instructions 1000 1000 lines)
        count-2 (run-instructions-2 1000 1000 lines)]
    (println "Day 06, part 1:" count)
    (println "Day 06, part 2:" count-2)))
