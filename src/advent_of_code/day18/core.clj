(ns advent-of-code.day18.core
  (:require
    [clojure.string :as str]))

(defn is-corner [grid x y]
  (let [max-x (- (count (get grid 0)) 1)
        max-y (- (count grid) 1)]
    (if (or (and (= 0 x) (= 0 y)) (and (= 0 x) (= max-y y)) (and (= max-x x) (= 0 y)) (and (= max-x x) (= max-y y)))
      true
      false)))

(defn light-state [grid x y stuck]
  (let [max-x (- (count (get grid 0)) 1)
        max-y (- (count grid) 1)]
    (if (or (< x 0) (< y 0) (> x max-x) (> y max-y))
      0
      (if (and stuck (is-corner grid x y))
        1
        (get-in grid [y x])))))

(defn neighbour-states [grid x y stuck]
  (for [x-diff [-1 0 1]
        y-diff [-1 0 1]
        :when (not (and (= 0 x-diff) (= 0 y-diff)))]
    (light-state grid (+ x x-diff) (+ y y-diff) stuck)))

(defn neighbours-off [grid x y stuck]
  (count (filter #(= 0 %) (neighbour-states grid x y stuck))))

(defn neighbours-on [grid x y stuck]
  (count (filter #(= 1 %) (neighbour-states grid x y stuck))))

(defn animate-light [grid x y stuck]
  (if (and stuck (is-corner grid x y))
    1
    (let [current-state (light-state grid x y stuck)
          ons (neighbours-on grid x y stuck)]
      (if (= 1 current-state)
        (if (or (= 2 ons) (= 3 ons))
          1
          0)
        (if (= 3 ons)
          1
          0)))))

(defn animate-row [grid y stuck]
  (let [row (get grid y)
        length (count row)]
    (loop [out []
            x 0]
      (if (= x length)
        out
        (recur (conj out (animate-light grid x y stuck)) (inc x))))))

(defn animate-grid [grid stuck]
  (let [length (count grid)]
    (loop [out []
            y 0]
      (if (= y length)
        out
        (recur (conj out (animate-row grid y stuck)) (inc y))))))

(defn animate-times [grid times stuck]
  (last (take (+ times 1) (iterate (fn [g] (animate-grid g stuck)) grid))))

(defn count-row-ons [row]
  (count (filter #(= 1 %) row)))

(defn count-ons [grid]
  (loop [out 0
          row (first grid)
          grid (rest grid)]
    (if (not row)
      out
      (recur (+ out (count-row-ons row)) (first grid) (rest grid)))))

(defn char->state [c]
  (if (= c \#)
    1
    0))

(defn line->row [line]
  (let [chars (seq line)]
    (loop [out []
            c (first chars)
            chars (rest chars)]
      (if (not c)
        out
        (recur (conj out (char->state c)) (first chars) (rest chars))))))

(defn lines->grid [lines]
  (mapv line->row lines))

(defn read-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->grid lines)))

(defn animate-file [file times stuck]
  (let [grid (read-file file)
        animated (animate-times grid times stuck)]
    (count-ons animated)))

(defn run []
  (println "Day 18, part 1:" (animate-file "src/advent_of_code/day18/input.txt" 100 false))
  (println "Day 18, part 2:" (animate-file "src/advent_of_code/day18/input.txt" 100 true)))
