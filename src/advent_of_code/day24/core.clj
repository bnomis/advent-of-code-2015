(ns advent-of-code.day24.core
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.math.combinatorics :as combo]))

(defn qe [group]
  (reduce * group))

(defn qe-of-shortest [group]
  (loop [shortest (first group)
          g (first group)
          group (rest group)]
    (if-not g
      (qe shortest)
      (if (< (count g) (count shortest))
        (recur g (first group) (rest group))
        (recur shortest (first group) (rest group))))))

(defn matches? [group target]
  (loop [g (first group)
          group (rest group)]
    (if-not g
      true
      (let [sum (reduce + g)]
        (if-not (= sum target)
          false
          (recur (first group) (rest group)))))))

(defn shortest-in [group]
  (loop [lens []
          g (first group)
          group (rest group)]
    (if-not g
      (apply min lens)
      (recur (conj lens (count g)) (first group) (rest group)))))

(defn shortest-group [groups]
  (loop [shortest (shortest-in (first groups))
          g (first groups)
          groups (rest groups)]
    (if-not g
      shortest
      (let [sg (shortest-in g)
            shortest (if (< sg shortest) sg shortest)]
        (recur shortest (first groups) (rest groups))))))

(defn contains-length [group length]
  (loop [g (first group)
          group (rest group)]
    (if-not g
      false
      (if (= length (count g))
        true
        (recur (first group) (rest group))))))

(def found-groups (atom []))

(defn reset-found-groups []
  (reset! found-groups []))

(defn add-to-found [group]
  (swap! found-groups conj group))

(defn partition-filter [weights target sub progress]
  (if (= 1 sub)
    (if (= target (reduce + weights))
      (add-to-found (conj progress weights)))
    (let [length (count weights)]
      (loop [l 1]
        (when (< l length)
          (doseq [g (combo/combinations (vec weights) l)]
            (if (= target (reduce + g))
              (partition-filter (set/difference (set weights) (set g)) target (- sub 1) (conj progress g))))
          (recur (inc l)))))))

(defn distribute-weights2 [weights sub]
  (let [sum (reduce + weights)
        target (/ sum sub)]
    (reset-found-groups)
    (partition-filter weights target sub [])
    (let [shortest (shortest-group @found-groups)
          parts (filter #(contains-length % shortest) @found-groups)
          shortest-qes (mapv qe-of-shortest parts)]
      (apply min shortest-qes))))

(defn partition-weights [weights sub]
  (combo/partitions weights :min sub :max sub))

(defn filter-matches [parts target]
  (filter #(matches? % target) parts))

(defn distribute-weights3 [weights sub]
  (let [sum (reduce + weights)
        target (/ sum sub)
        parts (partition-weights weights sub)
        parts (filter-matches parts target)
        shortest (shortest-group parts)
        parts (filter #(contains-length % shortest) parts)
        shortest-qes (mapv qe-of-shortest parts)]
    (apply min shortest-qes)))

(defn find-combos [weights target length]
  (for [g (combo/combinations weights length)
        :when (= target (reduce + g))]
        g))

(defn find-shortest-combos [weights target start]
  (let [length (count weights)]
    (loop [i start]
      (if (>= i length)
        nil
        (let [groups (find-combos weights target i)]
          (if (not-empty groups)
            groups
            (recur (inc i))))))))


;; this works and is quick but does not check the other groups in a set are good
(defn distribute-weights [weights sub]
  (let [sum (reduce + weights)
        target (/ sum sub)
        groups (find-shortest-combos weights target 1)
        shortest-qes (mapv qe groups)]
    (apply min shortest-qes)))

(defn read-weights [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    (loop [out []
            l (first lines)
            lines (rest lines)]
      (if-not l
        out
        (recur (conj out (Integer/parseInt l)) (first lines) (rest lines))))))

(defn run []
  (let [weights (read-weights "src/advent_of_code/day24/input.txt")]
    (println "Day 24, part 1:" (distribute-weights weights 3))
    (println "Day 24, part 2:" (distribute-weights weights 4))))
