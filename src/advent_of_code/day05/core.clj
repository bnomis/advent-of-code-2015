(ns advent-of-code.day05.core
  (:require [clojure.string :as str]))

(defn bad-string [input]
  (cond
    (= "ab" input) true
    (= "cd" input) true
    (= "pq" input) true
    (= "xy" input) true
    :else false))

(defn is-vowel [c]
    (cond
      (= \a c) true
      (= \e c) true
      (= \i c) true
      (= \o c) true
      (= \u c) true
      :else false))

(defn update-results [results last this]
  (let [keys []
        keys (if (= last this)
              (conj keys :repeat-count)
              keys)
        keys (if (is-vowel this)
              (conj keys :vowel-count)
              keys)]
    (reduce (fn [r k] (update r k inc)) results keys)))

(defn nice-string [input]
  (let [chars (seq input)
        results (loop [ last ""
                        this (first chars)
                        chars (rest chars)
                        results { :vowel-count 0
                                  :repeat-count 0
                                  :bad-string false}]
                  (if-not this
                    results
                    (do
                      (if (bad-string (str last this))
                        (assoc results :bad-string true)
                        (recur this (first chars) (rest chars) (update-results results last this))))))]
    ;;(println input results)
    (if (and (= (:bad-string results) false) (>= (:vowel-count results) 3) (>= (:repeat-count results) 1))
      true
      false)))

(defn run []
  (let [input (slurp "src/advent_of_code/day05/input.txt")
        lines (str/split input #"\n")
        count (reduce (fn [acc l]
                        (if (nice-string l)
                          (inc acc)
                          acc))
                0 lines)]
    (println "Day 05:" count)))
