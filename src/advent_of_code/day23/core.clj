(ns advent-of-code.day23.core
  (:require
    [clojure.string :as str]))

(defn reg-even? [registers reg]
  (even? (get registers (keyword reg))))

(defn reg-one? [registers reg]
  (= 1 (get registers (keyword reg))))

(defn remove-last-char [s]
  (let [parts (vec (seq s))
        length (count parts)]
    (str/join (subvec parts 0 (- length 1)))))

(defn jump-parse [reg off]
  [(remove-last-char reg) (Integer/parseInt off)])

(defn half [registers reg]
  (update registers (keyword reg) / 2))

(defn triple [registers reg]
  (update registers (keyword reg) * 3))

(defn incr [registers reg]
  (update registers (keyword reg) inc))

(defn run-program [program registers]
  (let [length (count program)
        max-index (- length 1)]
    (loop [index 0
            registers registers]
      (if (> index max-index)
        registers
        (let [tokes (str/split (get program index) #" ")
              op (get tokes 0)
              reg (get tokes 1)
              [off regs] (case op
                          "hlf" [1 (half registers reg)]
                          "tpl" [1 (triple registers reg)]
                          "inc" [1 (incr registers reg)]
                          "jmp" [(Integer/parseInt reg) registers]
                          "jie" (let [[reg off] (jump-parse reg (get tokes 2))]
                                  (if (reg-even? registers reg)
                                    [off registers]
                                    [1 registers]))
                          "jio" (let [[reg off] (jump-parse reg (get tokes 2))]
                                  (if (reg-one? registers reg)
                                    [off registers]
                                    [1 registers])))]
          (recur (+ index off) regs))))))

(defn run-read [program reg registers]
  (let [registers (run-program program registers)]
    (get registers reg)))

(defn read-program [file]
  (let [input (slurp file)]
    (str/split input #"\n")))

(defn run []
  (let [program (read-program "src/advent_of_code/day23/input.txt")]
    (println "Day 23, part 1:" (run-read program :b {:a 0 :b 0}))
    (println "Day 23, part 2:" (run-read program :b {:a 1 :b 0}))))
