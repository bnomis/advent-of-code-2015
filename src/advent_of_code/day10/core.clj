(ns advent-of-code.day10.core
  (:require [clojure.string :as str]))

(defn mk-run [ints index]
  (let [current (get ints index)
        length (count ints)]
    (loop [out [current]
            index (+ index 1)
            next (get ints index)]
      (if (or (> index (- length 1)) (not next) (not= current next))
        out
        (recur (conj out current) (inc index) (get ints (+ index 1)))))))

(defn runs [input]
  (let [chars (str/split input #"")
        ints (mapv #(Integer/parseInt %) chars)
        length (count ints)]
    (loop [runs []
            index 0]
      (if (> index (- length 1))
        runs
        (let [r (mk-run ints index)]
          (recur (conj runs r) (+ index (count r))))))))

(defn encode-run [run]
  (let [length (count run)]
    (apply str [length (first run)])))

(defn encode [input]
  (let [rs (runs input)]
    (loop [out []
            r (first rs)
            rs (rest rs)]
      (if-not r
        (apply str out)
        (recur (conj out (encode-run r)) (first rs) (rest rs))))))

(defn encode-loop [input count]
  (loop [count count
          input input]
    (if (= 0 count)
      input
      (recur (dec count) (encode input)))))


(defn run []
  (println "Day 10, part 1:" (count (encode-loop "1113122113" 40)))
  (println "Day 10, part 2:" (count (encode-loop "1113122113" 50))))
