(ns advent-of-code.day25.core
  (:require
    [clojure.string :as str]))

(defn next-coord [row column]
  (if (< (- row 1) 0)
    [(+ column 1) 0]
    [(- row 1) (+ column 1)]))

(defn prev-coord [row column]
  (if (< (- column 1) 0)
    [0 (- row 1)]
    [(+ row 1) (- column 1)]))

(defn num-rows [grid]
  (count grid))

(defn last-row-length [grid]
  (let [rows (num-rows grid)
        row (get grid (- rows 2))]
    (if row
      (count row)
      0)))

(defn add-row [grid]
  (conj grid []))

(defn read-code-at [grid row column]
  (get-in grid [row column]))

(defn set-code-at [grid row column value]
  (if (< (num-rows grid) (+ row 1))
    (let [grid (add-row grid)]
      (assoc-in grid [row column] value))
    (assoc-in grid [row column] value)))

(defn next-value [prev multy divy]
  (mod (* prev multy) divy))

(defn build-grid [starting-value multy divy size]
  (loop [grid [[starting-value]]
          prev-row 0
          prev-col 0
          prev-val starting-value]
    (if (and (> (num-rows grid) size) (>= (last-row-length grid) size))
      grid
      (let [value (next-value prev-val multy divy)
            [row col] (next-coord prev-row prev-col)]
        (recur (set-code-at grid row col value) row col value)))))

;; this builds the full grid... not needed
(defn find-value-at2 [starting-value multy divy row column]
  (let [row (- row 1)
        column (- column 1)
        biggest (max row column)
        grid (build-grid starting-value multy divy biggest)]
    (read-code-at grid row column)))

(defn find-value-at [starting-value multy divy row column]
  (let [target-row (- row 1)
        target-column (- column 1)]
    (loop [prev-row 0
            prev-col 0
            prev-val starting-value]
      (let [value (next-value prev-val multy divy)
            [row col] (next-coord prev-row prev-col)]
        (if (and (= row target-row) (= col target-column))
          value
          (recur row col value))))))

(defn remove-last-char [s]
  (let [parts (into [] (seq (str/trim s)))
        length (count parts)]
    (apply str (subvec parts 0 (- length 1)))))

(defn make-int [token]
  (let [token (remove-last-char token)]
    (Integer/parseInt token)))

(defn read-row-column [file]
  (let [input (slurp file)
        tokes (str/split input #" ")
        length (count tokes)]
    (loop [i 0
            row 0
            column 0]
      (if (= i length)
        [row column]
        (let [t (get tokes i)]
          (if (= "row" t)
            (recur (inc i) (make-int (get tokes (+ i 1))) column)
            (if (= "column" t)
              (recur (inc i) row (make-int (get tokes (+ i 1))))
              (recur (inc i) row column))))))))

(defn run []
  (let [[row column] (read-row-column "src/advent_of_code/day25/input.txt")
        starting-value 20151125
        multy 252533
        divy 33554393]
    (println "Day 25, part 1:" (find-value-at starting-value multy divy row column))))
