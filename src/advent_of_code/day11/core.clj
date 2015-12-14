(ns advent-of-code.day11.core
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

(def max-alpha-number 25)

(def password-length 8)

;; i, l, o
(def bad-ints #{8 11 14})

(def alphabet ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(def alpha-map {"a" 0 "b" 1 "c" 2 "d" 3 "e" 4 "f" 5 "g" 6 "h" 7 "i" 8 "j" 9 "k" 10 "l" 11 "m" 12
                "n" 13 "o" 14 "p" 15 "q" 16 "r" 17 "s" 18 "t" 19 "u" 20 "v" 21 "w" 22 "x" 23 "y" 24 "z" 25})

(defn letter->index [letter]
  (get alpha-map letter))

(defn index->letter [index]
  (nth alphabet index))

(defn str->nums [input]
  (let [letters (str/split input #"")]
    (loop [out []
            l (first letters)
            letters (rest letters)]
      (if-not l
        out
        (recur (conj out (letter->index l)) (first letters) (rest letters))))))

(defn nums->str [numbers]
  (loop [out []
          n (first numbers)
          numbers (rest numbers)]
    (if-not n
      (apply str out)
      (recur (conj out (index->letter n)) (first numbers) (rest numbers)))))

(defn inc-past-bad [in]
  (let [out (inc in)]
    (if (set/subset? #{out} bad-ints)
      (inc out)
      out)))

(defn inc-alpha-number [in]
  (let [out (inc-past-bad in)]
    (if (> out max-alpha-number)
      0
      out)))

(defn wrapped [old new]
  (< new old))

(defn inc-alpha-numbers [numbers]
  (let [numbers (reverse numbers)]
    (loop [out []
            old (first numbers)
            numbers (rest numbers)]
      (if-not old
        (into [] (reverse out))
        (do
          (let [new (inc-alpha-number old)
                did-wrap (wrapped old new)]
            (if (not did-wrap)
              (into [] (reverse (concat out [new] numbers)))
              (recur (conj out new) (first numbers) (rest numbers)))))))))

(defn has-bad-ints [numbers]
  (> (count (set/intersection bad-ints (into #{} numbers))) 0))

(defn is-straight [numbers]
  (let [length (count numbers)
        start (first numbers)
        out (take length (iterate inc start))]
    (= numbers out)))

(defn has-straight [numbers]
  (let [length (count numbers)]
    (loop [index 0]
      (if (>= index (- length 2))
        false
        (if (is-straight (subvec numbers index (+ index 3)))
          true
          (recur (inc index)))))))

(defn is-a-pair [numbers index]
  (= (get numbers index) (get numbers (inc index))))

(defn look-for-pair [numbers data]
  (let [length (count numbers)
        index (:index data)]
    (if (>= index (- length 1))
      data
      (do
        (if (is-a-pair numbers index)
          (-> data
            (update :index #(+ % 2))
            (update :pairs inc))
          (-> data
            (update :index inc)))))))

(defn has-two-pairs [numbers]
  (let [length (count numbers)]
    (loop [data {:index 0, :pairs 0}]
      (if (>= (:index data) (- length 1))
        (>= (:pairs data) 2)
        (recur (look-for-pair numbers data))))))

(defn good-password [numbers]
  (and (not (has-bad-ints numbers)) (has-straight numbers) (has-two-pairs numbers)))

(defn good-password-str [input]
  (good-password (str->nums input)))

(defn next-password [start]
  (let [numbers (str->nums start)]
    (loop [numbers (inc-alpha-numbers numbers)]
      ;;(println numbers)
      (if (good-password numbers)
        (nums->str numbers)
        (recur (inc-alpha-numbers numbers))))))

(defn run []
  (println "Day 11, part 1" (next-password "cqjxjnds"))
  (println "Day 11, part 2" (next-password "cqjxxyzz")))
