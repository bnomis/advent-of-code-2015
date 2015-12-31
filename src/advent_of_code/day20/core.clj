(ns advent-of-code.day20.core
  (:require
    [clojure.set :as set]))

(defn elves-for-house [h]
  (let [max (/ h 2)
        all (range 1 max)]
    (loop [out []
            e 1]
      (if (> e max)
        (conj out h)
        (if (= 0 (mod h e))
          (recur (conj out e) (inc e))
          (recur out (inc e)))))))

(defn sum [n]
  (/ (* n (+ n 1)) 2))

(defn can-factor [h i]
  (if (= 0 (mod h i))
    true
    false))

(defn factor-by [h i]
  (set/union #{i} #{(/ h i)}))

(defn is-a-factor [factors i]
  (set/subset? #{i} factors))

(defn factor [h]
  (let [max (/ h 2)]
    (loop [factors (set/union #{1} #{h})
            i 2]
      (if (or (> i max) (is-a-factor factors i))
        factors
        (if (can-factor h i)
          (recur (set/union factors (factor-by h i)) (inc i))
          (recur factors (inc i)))))))

(defn presents-for-house [h]
  (let [elves (factor h)]
    (* 10 (reduce + elves))))

;; try to find lower bound by summing integers which must be more than the sum of factors
(defn house-range [p]
  (loop [h 1]
    (if (>= (sum h) p)
      h
      (recur (* h 2)))))

(defn house-with-at-least-presents-slow [p]
  (let [p (/ p 10)
        start (house-range p)]
    (loop [h start]
      (if (>= (presents-for-house h) p)
        h)
      (recur (inc h)))))

(defn present-sequence [p]
  (let [p (/ p 10)]
    (for [elf (range 1 p)
          house (range elf p elf)]
      [house elf])))

(defn present-totals [p]
  (let [ps (present-sequence p)]
    (loop [totals {}
            h (first ps)
            ps (rest ps)]
      (if-not h
        totals
        (let [house (get h 0)
              presents (get h 1)
              totals (update totals house (fnil + 0) presents)]
            (recur totals (first ps) (rest ps)))))))

(defn house-with-at-least-presents [p]
  (let [totals (present-totals p)
        p (/ p 10)]
    (loop [i 1]
      (when-not (>= i p)
        (if (>= (get totals i) p)
          i
          (recur (inc i)))))))

(defn update-totals [totals presents-per-elf houses-per-elf elf]
  (let [max (- houses-per-elf 1)
        presents-per (* presents-per-elf elf)]
    (loop [totals totals
            counter 0]
      (if (> counter max)
        totals
        (recur (update totals (+ elf (* counter elf)) (fnil + 0) presents-per) (inc counter))))))

(defn check-totals [totals houses-per-elf elf target]
  (let [max (- houses-per-elf 1)]
    (loop [counter 0]
      (when-not (> counter max)
        (let [house (+ elf (* counter elf))]
          (if (>= (get totals house) target)
            house
            (recur (inc counter))))))))

(defn house-with-at-least-presents2 [target presents-per-elf houses-per-elf]
  (loop [ totals {}
          elf 1]
    (let [totals (update-totals totals presents-per-elf houses-per-elf elf)]
      (if (>= (get totals elf) target)
        elf
        (recur totals (inc elf))))))

(defn run []
  (println "Day 20, part 1:" (house-with-at-least-presents 33100000))
  (println "Day 20, part 2:" (house-with-at-least-presents2 33100000 11 50)))
