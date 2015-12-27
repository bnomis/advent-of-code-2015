(ns advent-of-code.day12.core
  (:require
    [clojure.java.io :as io]
    [clojure.data.json :as json]))

(def eof -1)
(def minus-character 45)
(def zero 48)
(def nine 57)

(defn is-negative [c]
  (= c minus-character))

(defn is-number [c]
  (and (>= c zero) (<= c nine)))

(defn read-file [file]
  (with-open
    [xin (io/input-stream file)]
    (loop [c (.read xin)
            out []]
      (if (= c eof)
        out
        (recur (.read xin) (conj out c))))))

(defn eat-number [file-bytes index]
  (let [length (count file-bytes)
        mult (if (is-negative (nth file-bytes (- index 1))) -1 1)]
    (loop [parts []
            index index]
      (let [p (nth file-bytes index)]
        (if (or (not (is-number p)) (>= index (- length 1)))
          [(* mult (Integer/parseInt (apply str parts))) (count parts)]
          (recur (conj parts (- p zero)) (inc index)))))))

(defn read-numbers [file]
  (let [file-bytes (read-file file)
        length (count file-bytes)]
    (loop [index 0
            numbers []]
      (if (>= index (- length 1))
        (reduce + numbers)
        (if (is-number (nth file-bytes index))
          (let [[number number-length] (eat-number file-bytes index)]
            ;;(println number number-length)
            (recur (+ index number-length) (conj numbers number)))
          (recur (inc index) numbers))))))

(defn is-red [data]
  (let [values (vals data)]
    (loop [v (first values)
            values (rest values)]
      (if-not v
        false
        (if (and (string? v) (= v "red"))
          true
          (recur (first values) (rest values)))))))

(declare walk-vector)

(defn walk-map [data]
  (if (is-red data)
    []
    (let [keys (keys data)]
      (loop [out []
              k (first keys)
              keys (rest keys)]
        (if-not k
          out
          (let [d (get data k)]
            (cond
              (integer? d) (recur (conj out d) (first keys) (rest keys))
              (vector? d) (recur (concat out (walk-vector d)) (first keys) (rest keys))
              (map? d) (recur (concat out (walk-map d)) (first keys) (rest keys))
              :else (recur out (first keys) (rest keys)))))))))

(defn walk-vector [data]
  (loop [out []
          d (first data)
          data (rest data)]
    (if-not d
      out
      (cond
        (integer? d) (recur (conj out d) (first data) (rest data))
        (vector? d) (recur (concat out (walk-vector d)) (first data) (rest data))
        (map? d) (recur (concat out (walk-map d)) (first data) (rest data))
        :else (recur out (first data) (rest data))))))

(defn structured-read [file]
  (let [data (json/read-str (slurp file))
        numbers (walk-vector data)]
    (reduce + numbers)))

(defn run []
  (println "Day 12, part 1:" (read-numbers "src/advent_of_code/day12/input.txt"))
  (println "Day 12, part 2:" (structured-read "src/advent_of_code/day12/input.txt")))
