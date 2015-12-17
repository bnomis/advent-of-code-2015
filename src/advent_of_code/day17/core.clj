(ns advent-of-code.day17.core
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]))

(def alphabet ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(defn letter->capacity [normalised letter]
  (get normalised letter))

(defn combo->capacity [normalised combo]
  (loop [caps []
          c (first combo)
          combo (rest combo)]
    (if (not c)
      (reduce + caps)
      (recur (conj caps (letter->capacity normalised c)) (first combo) (rest combo)))))

(defn index->letter [index]
  (keyword (get alphabet index)))

(defn normalise-containers [containers]
  (loop [out {}
          index 0
          c (first containers)
          containers (rest containers)]
    (if (not c)
      out
      (recur (assoc out (index->letter index) c) (inc index) (first containers) (rest containers)))))

(defn find-combos [normalised target t]
  (let [letters (keys normalised)
        combos (combo/combinations letters t)]
    (filter (fn [c] (= target (combo->capacity normalised c))) combos)))

(defn find-combinations [containers target]
  (let [normalised (normalise-containers containers)
        length (count containers)]
    (loop [combos []
            t 1]
      (if (> t length)
        combos
        (recur (concat combos (find-combos normalised target t)) (inc t))))))

(defn find-combinations-minimum [containers target]
  (let [normalised (normalise-containers containers)
        length (count containers)]
    (loop [combos []
            t 1]
      (if (or (> (count combos) 0) (> t length))
        combos
        (recur (find-combos normalised target t) (inc t))))))

(defn line->container [line]
  (Integer/parseInt line))

(defn lines->containers [lines]
  (mapv line->container lines))

(defn read-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->containers lines)))

(defn find-target-combos [file target]
  (let [containers (read-file file)]
    (find-combinations containers target)))

(defn find-target-combos-minimum [file target]
  (let [containers (read-file file)]
    (find-combinations-minimum containers target)))

(defn run []
  (println "Day 17, part 1:" (count (find-target-combos "src/advent_of_code/day17/input.txt" 150)))
  (println "Day 17, part 2:" (count (find-target-combos-minimum "src/advent_of_code/day17/input.txt" 150))))
