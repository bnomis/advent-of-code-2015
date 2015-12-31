(ns advent-of-code.day21.core
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.math.combinatorics :as combo]))

;; data structures
;; player {:damage :armor :hit-points :cost}

(def combos { :weapons [1]
              :armor [0 1]
              :rings [0 1 2]})

(def shop {   :weapons [  {:name "Dagger" :cost 8 :damage 4 :armor 0}
                          {:name "Shortsword" :cost 10 :damage 5 :armor 0}
                          {:name "Warhammer" :cost 25 :damage 6 :armor 0}
                          {:name "Longsword" :cost 40 :damage 7 :armor 0}
                          {:name "Greataxe" :cost 74 :damage 8 :armor 0}]
              :armor [    {:name "Leather" :cost 13 :damage 0 :armor 1}
                          {:name "Chainmail" :cost 31 :damage 0 :armor 2}
                          {:name "Splintmail" :cost 53 :damage 0 :armor 3}
                          {:name "Bandedmail" :cost 75 :damage 0 :armor 4}
                          {:name "Platemail" :cost 102 :damage 0 :armor 5}]
              :rings [    {:name "Damage-1" :cost 25 :damage 1 :armor 0}
                          {:name "Damage-2" :cost 50 :damage 2 :armor 0}
                          {:name "Damage-3" :cost 100 :damage 3 :armor 0}
                          {:name "Defense-1" :cost 20 :damage 0 :armor 1}
                          {:name "Defense-2" :cost 40 :damage 0 :armor 2}
                          {:name "Defense-3" :cost 80 :damage 0 :armor 3}]})

(defn damage-per-turn [from to]
  (max (- (:damage from)  (:armor to)) 1))

(defn turns-to-zero [from to]
  (let [damage (damage-per-turn from to)
        points (:hit-points to)
        turns (int (/ points damage))]
    (if (> (mod points damage) 0)
      (+ turns 1))
    turns))

(defn winner [player boss]
  (let [player-turns (turns-to-zero player boss)
        boss-turns (turns-to-zero boss player)]
    (if (< player-turns boss-turns)
      :player
      (if (< boss-turns player-turns)
        :boss
        :player))))

(defn player-wins? [player boss]
  (= :player (winner player boss)))

(defn player-loses? [player boss]
  (= :boss (winner player boss)))

(defn make-player [items]
  ;;(println "make-player:" items)
  (loop [player {:hit-points 100 :damage 0 :armor 0 :cost 0}
          i (first items)
          items (rest items)]
    (if-not i
      player
      (recur (-> player (update :damage + (:damage i)) (update :armor + (:armor i)) (update :cost + (:cost i))) (first items) (rest items)))))

(defn find-outcome [boss configs cmp]
  (loop [wins []
          items (first configs)
          configs (rest configs)]
    (if-not items
      wins
      (let [player (make-player items)]
        (if (cmp player boss)
          (recur (conj wins player) (first configs) (rest configs))
          (recur wins (first configs) (rest configs)))))))

(defn make-seq [number category]
  (let [length (count (get shop category))]
    (case number
      0 [-1]
      1 (range length)
      2 (combo/combinations (range length) 2)
      nil)))

(defn get-weapon [weapon]
  (when (and weapon (> weapon -1))
    (nth (:weapons shop) weapon)))

(defn get-armor [armor]
  (when (and armor (> armor -1))
    (nth (:armor shop) armor)))

(defn get-rings [rings]
  (if (seq? rings)
    (loop [out []
            r (first rings)
            rings (rest rings)]
      (if-not r
        out
        (recur (conj out (nth (:rings shop) r)) (first rings) (rest rings))))
    (when (and rings (> rings -1))
      [(nth (:rings shop) rings)])))

(defn build-config [[weapon armor rings]]
  ;;(println "build-config:" weapon armor rings)
  (let [items (-> [] (conj (get-weapon weapon)) (conj (get-armor armor)) (concat (get-rings rings)))]
    ;;(println items)
    (filter some? items)))

(defn make-config [[weapons armor rings]]
  (let [weapons-seq (make-seq weapons :weapons)
        armor-seq (make-seq armor :armor)
        rings-seq (make-seq rings :rings)
        carts (combo/cartesian-product weapons-seq armor-seq rings-seq)]
    ;;(println "weapons" weapons-seq)
    ;;(println "armor" armor-seq)
    ;;(println "rings" rings-seq)
    ;;(println "carts" carts)
    (loop [out []
            c (first carts)
            carts (rest carts)]
      (if-not c
        out
        (recur (conj out (build-config c)) (first carts) (rest carts))))))

(defn make-configs []
  (let [carts (combo/cartesian-product (:weapons combos) (:armor combos) (:rings combos))]
    (loop [out []
            c (first carts)
            carts (rest carts)]
      (if-not c
        out
        (recur (concat out (make-config c)) (first carts) (rest carts))))))

(defn lowest-cost-win [configs boss]
  (let [wins (find-outcome boss configs player-wins?)
        wins (sort-by :cost wins)]
    (:cost (first wins))))

(defn highest-cost-loss [configs boss]
  (let [loses (find-outcome boss configs player-loses?)
        loses (sort-by :cost loses)]
    (:cost (last loses))))

(defn line->boss [boss line]
  (let [tokes (str/split line #":")
        val (Integer/parseInt (str/trim (get tokes 1)))]
    (case (first tokes)
      "Hit Points" (assoc boss :hit-points val)
      "Damage" (assoc boss :damage val)
      "Armor" (assoc boss :armor val)
      boss)))

(defn lines->boss [lines]
  (loop [boss {}
          l (first lines)
          lines (rest lines)]
    (if-not l
      boss
      (recur (line->boss boss l) (first lines) (rest lines)))))

(defn read-boss [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->boss lines)))

(defn run []
  (let [boss (read-boss "src/advent_of_code/day21/input.txt")
        configs (make-configs)]
    (println "Day 21, part 1:" (lowest-cost-win configs boss))
    (println "Day 21, part 2:" (highest-cost-loss configs boss))))
