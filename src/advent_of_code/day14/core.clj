(ns advent-of-code.day14.core
  (:require
    [clojure.string :as str]))

;; data structures

;; reindeer
;; {:name "Comet" :speed 14 :fly 10 :rest 127}

(defn traveled [reindeer time]
  (let [speed (:speed reindeer)
        fly (:fly reindeer)
        rest (:rest reindeer)
        fly-rest (+ fly rest)
        fly-rest-units (int (/ time fly-rest))
        remainder (- time (* fly-rest fly-rest-units))
        extra (if (> remainder fly) fly remainder)]
    (+ (* speed fly fly-rest-units) (* speed extra))))

(defn distances [reindeer time]
  (mapv #(traveled % time) reindeer))

(defn furthest [distances]
  (last (sort distances)))

(defn move-reindeer [reindeer time]
  (assoc reindeer :distance (traveled reindeer time)))

(defn distance-for [reindeer time]
  (loop [out []
          r (first reindeer)
          reindeer (rest reindeer)]
    (if-not r
      out
      (recur (conj out (move-reindeer r time)) (first reindeer) (rest reindeer)))))

(defn add-points-if-max [reindeer max-distance]
  (if (= (:distance reindeer) max-distance)
    (update reindeer :points inc)
    reindeer))

(defn travel-for [reindeer time]
  (let [updated (distance-for reindeer time)
        distances-travelled (mapv :distance updated)
        max-distance (apply max distances-travelled)]
    (loop [out []
            r (first updated)
            updated (rest updated)]
      (if-not r
        out
        (recur (conj out (add-points-if-max r max-distance)) (first updated) (rest updated))))))

(defn points [reindeer time]
  (loop [reindeer reindeer
          t 1]
    (if (> t time)
      (mapv :points reindeer)
      (recur (travel-for reindeer t) (inc t)))))

(defn line->reindeer [line]
  (let [tokes (str/split line #" ")
        name (get tokes 0)
        speed (Integer/parseInt (get tokes 3))
        fly (Integer/parseInt (get tokes 6))
        rest (Integer/parseInt (get tokes 13))]
    {:name name :speed speed :fly fly :rest rest :points 0 :distance 0}))

(defn lines->reindeer [lines]
  (mapv line->reindeer lines))

(defn read-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->reindeer lines)))

(defn process-file [file time]
  (let [reindeer (read-file file)
        dist (distances reindeer time)
        furth (furthest dist)]
    furth))

(defn process-points [file time]
  (let [reindeer (read-file file)
        pts (points reindeer time)
        furth (furthest pts)]
    furth))

(defn run []
  (println "Day 14, part 1:" (process-file "src/advent_of_code/day14/input.txt" 2503))
  (println "Day 14, part 2:" (process-points "src/advent_of_code/day14/input.txt" 2503)))
