(ns advent-of-code.day16.core
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(def mfcsam
  { :children 3
    :cats 7
    :samoyeds 2
    :pomeranians 3
    :akitas 0
    :vizslas 0
    :goldfish 5
    :trees 3
    :cars 2
    :perfumes 1})

(def greater #{:cats :trees})

(def fewer #{:pomeranians :goldfish})

(defn abs-diff [a b]
  (Math/abs (- a b)))

(defn abs-diff-sue-retro [sue key]
  (let [ks #{key}
        sue-value (get sue key)
        mfcsam-value (get mfcsam key)]
    (cond
      (set/subset? ks greater) (if (> sue-value mfcsam-value) 0 1)
      (set/subset? ks fewer) (if (< sue-value mfcsam-value) 0 1)
      :else (abs-diff sue-value mfcsam-value))))

(defn abs-diff-sue [sue key retro]
  (if (= key :sue)
    0
    (if retro
      (abs-diff-sue-retro sue key)
      (abs-diff (get sue key) (get mfcsam key)))))

(defn sue->score [sue retro]
  (let [keys (keys sue)]
    (loop [scores []
            k (first keys)
            keys (rest keys)]
      (if (not k)
        (reduce + scores)
        (recur (conj scores (abs-diff-sue sue k retro)) (first keys) (rest keys))))))

(defn sues->scores [sues retro]
  (loop [out []
          s (first sues)
          sues (rest sues)]
    (if (not s)
      out
      (recur (conj out {:score (sue->score s retro) :sue s}) (first sues) (rest sues)))))

(defn sort-sues [sues]
  (sort-by :score sues))

(defn remove-last-char [s]
  (let [parts (into [] (seq s))
        length (count parts)]
    (apply str (subvec parts 0 (- length 1)))))

(defn remove-trailing-comma [s]
  (let [parts (into [] (seq s))
        length (count parts)]
    (if (= (last parts) \,)
      (apply str (subvec parts 0 (- length 1)))
      s)))

(defn extract-thing [out [key value]]
  (let [key (keyword (remove-last-char key))
        value (Integer/parseInt (remove-trailing-comma value))]
    (assoc out key value)))

(defn things->sue [things]
  (loop [out {}
          t (first things)
          things (rest things)]
    (if (not t)
      out
      (recur (extract-thing out t) (first things) (rest things)))))

(defn line->sue [line]
  (let [tokes (str/split line #" ")
        things (partition 2 (subvec tokes 2))
        sue (things->sue things)]
    (assoc sue :sue (Integer/parseInt (remove-last-char (get tokes 1))))))

(defn lines->sues [lines]
  (mapv line->sue lines))

(defn read-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->sues lines)))

(defn find-sue [file retro]
  (let [sues (read-file file)
        scored (sues->scores sues retro)
        sorted (sort-sues scored)]
    (:sue (first sorted))))

(defn run []
  (println "Day 16, part 1:" (find-sue "src/advent_of_code/day16/input.txt" false))
  (println "Day 16, part 2:" (find-sue "src/advent_of_code/day16/input.txt" true)))
