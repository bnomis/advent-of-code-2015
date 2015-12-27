(ns advent-of-code.day07.core
  (:require [clojure.string :as str]))

(defn is-int [in]
  (try
    (do
      (Integer/parseInt in)
      true)
    (catch Exception e
      false)))

(defn variablise [var]
  (if (is-int var)
    (Integer/parseInt var)
    (keyword var)))

(defn santa-assign [results in out]
  (let [out (keyword out)]
    (if (is-int in)
      (assoc results out (Integer/parseInt in))
      (assoc results out [:assign (keyword in)]))))

(defn santa-and [results left right out]
  (let [out (keyword out)
        left (variablise left)
        right (variablise right)]
    (assoc results out [:and left right])))

(defn santa-or [results left right out]
  (let [out (keyword out)
        left (variablise left)
        right (variablise right)]
    (assoc results out [:or left right])))

(defn santa-not [results in out]
  (let [out (keyword out)
        in (variablise in)]
    (assoc results out [:not in])))

(defn santa-lshift [results left right out]
  (let [out (keyword out)
        left (variablise left)
        right (variablise right)]
    (assoc results out [:lshift left right])))

(defn santa-rshift [results left right out]
  (let [out (keyword out)
        left (variablise left)
        right (variablise right)]
    (assoc results out [:rshift left right])))

(defn santa-gate [results left op right out]
  (case op
    "AND" (santa-and results left right out)
    "OR"  (santa-or  results left right out)
    "LSHIFT" (santa-lshift results left right out)
    "RSHIFT" (santa-rshift results left right out)
    nil))

(defn can-resolve [results var]
  (if-not (vector? (get results var))
    true
    false))

(defn resolve-var [results var]
  (if (keyword? var)
    (if (can-resolve results var)
      (get results var)
      var)
    var))

(defn resolve-assign [results left]
  (let [resolved-left (resolve-var results left)]
    (if (keyword? resolved-left)
      [:assign resolved-left]
      resolved-left)))

(defn resolve-and [results left right]
  (let [resolved-left (resolve-var results left)
        resolved-right (resolve-var results right)]
    (if (or (keyword? resolved-left) (keyword? resolved-right))
      [:and resolved-left resolved-right]
      (bit-and resolved-left resolved-right))))

(defn resolve-or [results left right]
  (let [resolved-left (resolve-var results left)
        resolved-right (resolve-var results right)]
    (if (or (keyword? resolved-left) (keyword? resolved-right))
      [:or resolved-left resolved-right]
      (bit-or resolved-left resolved-right))))

(defn resolve-lshift [results left right]
  (let [resolved-left (resolve-var results left)
        resolved-right (resolve-var results right)]
    (if (or (keyword? resolved-left) (keyword? resolved-right))
      [:lshift resolved-left resolved-right]
      (bit-and 0xffff (bit-shift-left resolved-left resolved-right)))))

(defn resolve-rshift [results left right]
  (let [resolved-left (resolve-var results left)
        resolved-right (resolve-var results right)]
    (if (or (keyword? resolved-left) (keyword? resolved-right))
      [:rshift resolved-left resolved-right]
      (bit-and 0xffff (bit-shift-right resolved-left resolved-right)))))

(defn resolve-not [results left]
  (let [resolved-left (resolve-var results left)]
    (if (keyword? resolved-left)
      [:not resolved-left]
      (bit-and 0xffff (bit-not resolved-left)))))

(defn resolve-vars [results value]
  (case (first value)
    :assign (resolve-assign results (nth value 1))
    :and (resolve-and results (nth value 1) (nth value 2))
    :or (resolve-or results (nth value 1) (nth value 2))
    :not (resolve-not results (nth value 1))
    :lshift (resolve-lshift results (nth value 1) (nth value 2))
    :rshift (resolve-rshift results (nth value 1) (nth value 2))
    nil))

(defn resolve-key [results out k]
  (let [value (get results k)]
    (if (vector? value)
      (assoc out k (resolve-vars results value))
      (assoc out k value))))

(defn ripple [results]
  ;;(println "ripple:" results)
  (let [keys (keys results)]
    (loop [out {}
            k (first keys)
            keys (rest keys)]
      (if-not k
        out
        (recur (resolve-key results out k) (first keys) (rest keys))))))

(defn all-resolved [results]
  (let [keys (keys results)]
    (loop [k (first keys)
            keys (rest keys)]
      (if-not k
        true
        (do
          (if (vector? (get results k))
            false
            (recur (first keys) (rest keys))))))))

(defn resolve-circuit [results]
  (loop [results results]
    ;;(println "resolve-circuit:" results)
    (if (all-resolved results)
      (do
        ;;(println "all resolved")
        results)
      (recur (ripple results)))))

(defn parse-line [results line]
  (let [tokes (str/split line #" ")
        length (count tokes)]
    (case length
      3 (santa-assign results (get tokes 0) (get tokes 2))
      4 (santa-not results (get tokes 1) (get tokes 3))
      5 (santa-gate results (get tokes 0) (get tokes 1) (get tokes 2) (get tokes 4))
      nil)))

(defn parse-circuit [lines]
  (loop [results {}
          l (first lines)
          lines (rest lines)]
    ;;(println "parse-circuit:" results l)
    (if-not l
      results
      (recur (parse-line results l) (first lines) (rest lines)))))

(defn run-circuit [lines]
  (resolve-circuit (parse-circuit lines)))

(defn print-results [results]
  (let [keys (sort (keys results))]
    (doseq [k keys]
      (println k (get results k)))))

(defn run-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")
        results (run-circuit lines)]
    (:a results)))

(defn run []
  (println "Day 07, part 1:" (run-file "src/advent_of_code/day07/input.txt"))
  (println "Day 07, part 2:" (run-file "src/advent_of_code/day07/input-2.txt")))
