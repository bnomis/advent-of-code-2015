(ns advent-of-code.day13.core
  (:require
    [clojure.string :as str]
    [clojure.math.combinatorics :as combo]))

;; data structures

;; links
;; [:Alice :Bob 54]
;; [:Alice :Carol -79]

;; neighbours
;; [:Alice :Bob]
;; [:Alice :David]

;; normailised
;; {:Alice {:Bob 54, :Carol -79}, ...}

;; seating arrangements
;; wrap around
;; [:Alice :Bob :Carol :David]

;; seating cost
;; [{:cost 55 :arrangement [:Alice :Bob :Carol :David]} ...]

(defn neighbour-cost [normalised neighbour]
  (get-in normalised neighbour))

(defn neighbours-in-arrangement [arrangement index]
  (let [length (count arrangement)
        center (nth arrangement index)
        left (if (= index 0) (- length 1) (- index 1))
        right (if (= index (- length 1)) 0 (+ index 1))]
    [[center (nth arrangement left)] [center (nth arrangement right)]]))

(defn neighbours [arrangement]
  (let [length (count arrangement)]
    (loop [out []
            index 0]
      (if (> index (- length 1))
        out
        (recur (concat out (neighbours-in-arrangement arrangement index)) (inc index))))))

(defn arrangement-cost [normalised arrangement]
  (reduce + (mapv #(neighbour-cost normalised %) (neighbours arrangement))))

(defn cost-arrangements [normalised arrangements]
  ;;(println "cost-arrangements:" arrangements)
  (loop [out []
          a (first arrangements)
          arrangements (rest arrangements)]
    (if-not a
      out
      (recur (conj out {:cost (arrangement-cost normalised a), :arrangement a}) (first arrangements) (rest arrangements)))))

(defn sort-arrangements [arrangements]
  ;;(println "sort-arrangements:" arrangements)
  (sort-by :cost arrangements))

(defn guest-list [normalised]
  ;;(println "guest-list:" normalised)
  (into [] (keys normalised)))

(defn make-arrangements [guests]
  ;;(println "make-arrangements:" guests)
  (into [] (combo/permutations guests)))

(defn add-link [out [person1 person2 cost]]
  (let [path [person1 person2]]
    (assoc-in out path cost)))

(defn normalise-links [links]
  ;;(println "normalise-links:" links)
  (loop [out {}
          l (first links)
          links (rest links)]
    (if-not l
      out
      (recur (add-link out l) (first links) (rest links)))))

(defn remove-last-char [s]
  (let [parts (into [] (seq s))
        length (count parts)]
    (apply str (subvec parts 0 (- length 1)))))

(defn line->link [line]
  (let [tokes (str/split line #" ")
        person1 (keyword (get tokes 0))
        person2 (keyword (remove-last-char (get tokes 10)))
        cost (Integer/parseInt (get tokes 3))
        sign (get tokes 2)
        mult (if (= sign "gain") 1 -1)]
    [person1 person2 (* mult cost)]))

(defn lines->links [lines]
  (mapv line->link lines))

(defn read-links [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (lines->links lines)))

(defn most-happy [sorted]
  (last sorted))

(defn least-happy [sorted]
  (first sorted))

(defn process-file [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        guests (guest-list normalised)
        arrangements (make-arrangements guests)
        costed (cost-arrangements normalised arrangements)
        sorted (sort-arrangements costed)]
    sorted))

(defn add-me-to-normalised [normalised]
  (let [keys (keys normalised)]
    (loop [out normalised
            k (first keys)
            keys (rest keys)]
      (if-not k
        out
        (recur (-> out (assoc-in [k :Me] 0) (assoc-in [:Me k] 0)) (first keys) (rest keys))))))

(defn process-file [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        guests (guest-list normalised)
        arrangements (make-arrangements guests)
        costed (cost-arrangements normalised arrangements)
        sorted (sort-arrangements costed)]
    sorted))

(defn process-file-add-me [file]
  (let [links (read-links file)
        normalised (normalise-links links)
        normalised (add-me-to-normalised normalised)
        guests (guest-list normalised)
        arrangements (make-arrangements guests)
        costed (cost-arrangements normalised arrangements)
        sorted (sort-arrangements costed)]
    sorted))

(defn run []
  (println "Day 13, part 1:" (:cost (most-happy (process-file "src/advent_of_code/day13/input.txt"))))
  (println "Day 13, part 2:" (:cost (most-happy (process-file-add-me "src/advent_of_code/day13/input.txt")))))
