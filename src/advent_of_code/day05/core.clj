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
                    (if (bad-string (str last this))
                      (assoc results :bad-string true)
                      (recur this (first chars) (rest chars) (update-results results last this)))))]
    ;;(println input results)
    (if (and (= (:bad-string results) false) (>= (:vowel-count results) 3) (>= (:repeat-count results) 1))
      true
      false)))

(defn match [p pairs]
  (loop [l (first pairs)
          pairs (rest pairs)]
    ;;(println "matching:" l p)
    (if-not l
      false
      (if (= l p)
        true
        (recur (first pairs) (rest pairs))))))

(defn check-pairs [pairs]
  ;;(println "check-pairs:" pairs)
  (loop [p (first pairs)
          pairs (rest pairs)]
    (if-not p
      nil
      ;; no overlap
      (if (match p (rest pairs))
        p
        (recur (first pairs) (rest pairs))))))

(defn join-two [c1 c2]
  (str/join [(str c1) (str c2)]))

(defn make-pairs [chars]
  (let [chars (mapv str chars)
        length (count chars)]
    (loop [pairs [(join-two (nth chars 0) (nth chars 1))]
            index 1]
      (if (>= index (- length 1))
        pairs
        (recur (conj pairs (join-two (nth chars index) (nth chars (+ 1 index)))) (inc index))))))

(defn has-pair [input]
  (let [p (check-pairs (make-pairs (seq input)))]
    (or p false)))

(defn has-pair2 [input]
  (let [p (check-pairs (make-pairs (seq input)))]
    (or p (let [p (check-pairs (make-pairs (rest (seq input))))]
            (or p false)))))

(defn has-repeat [input]
  (let [chars (seq input)
        length (count chars)]
    (loop [index 0]
      (if (> index (- length 3))
        false
        (if (= (nth chars index) (nth chars (+ index 2)))
          index
          (recur (inc index)))))))

(defn nice-string-2 [input]
  (let [p (has-pair input)
        r (has-repeat input)]
    (if (and p r)
      true
      false)))

(defn run []
  (let [input (slurp "src/advent_of_code/day05/input.txt")
        lines (str/split input #"\n")
        count (reduce (fn [acc l]
                        (if (nice-string l)
                          (inc acc)
                          acc))
                0 lines)
        count-2 (reduce (fn [acc l]
                          (if (nice-string-2 l)
                            (inc acc)
                            acc))
                  0 lines)]
    (println "Day 05, part 1:" count)
    (println "Day 05, part 2:" count-2)))
