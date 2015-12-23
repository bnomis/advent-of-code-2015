(ns advent-of-code.day19.core
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

;; In part 2 of this question, when reducing a long string to a single character,
;; the code runs for a very long time to consider all the possibilities and on my
;; machine runs out of memory. So we print out the number of steps found for each
;; reduction and find that it converges on the same value. This converged value is the
;; answer.

(defn matched [chars index from]
  (let [rchars (str/split from #"")
        length (count rchars)]
    (loop [rindex 0]
      (if (>= rindex length)
        true
        (if-not (= (get chars (+ index rindex)) (get rchars rindex))
          false
          (recur (inc rindex)))))))

(defn do-replace [out chars index from to]
  (let [from-length (count from)
        to-vec (str/split to #"")
        before (subvec chars 0 index)
        after (subvec chars (+ index from-length))
        parts (concat before to after)]
    (conj out (apply str parts))))

(defn replace-do [start replacement]
  (let [length (count start)
        from (get replacement 0)
        to (get replacement 1)
        chars (str/split start #"")]
    (loop [index 0
            out #{}]
      (if (>= index length)
        out
        (if (matched chars index from)
          (recur (inc index) (do-replace out chars index from to))
          (recur (inc index) out))))))

(defn replacer [start replacements]
  (loop [out #{}
          r (first replacements)
          replacements (rest replacements)]
    (if-not r
      out
      (recur (set/union out (replace-do start r)) (first replacements) (rest replacements)))))

(defn flip-replacements [replacements]
  (loop [out []
          r (first replacements)
          replacements (rest replacements)]
    (if-not r
      out
      (recur (conj out [(get r 1) (get r 0)]) (first replacements) (rest replacements)))))

(defn do-reduce [start repl]
  (str/replace-first start (get repl 0) (get repl 1)))

(defn can-reduce [start repl]
  (let [new (do-reduce start repl)]
    (if (not= start new)
      true
      false)))

(def reductions-atom (atom #{}))

(defn add-to-reductions [s]
  (swap! reductions-atom conj s))

(defn seen-reduction? [s]
  (set/subset? #{s} @reductions-atom))

(defn make-reductions [start replacements]
  (loop [out #{}
          r (first replacements)
          replacements (rest replacements)]
    (if-not r
      out
      (if (can-reduce start r)
        (let [new (do-reduce start r)]
          (recur (conj out new) (first replacements) (rest replacements)))
        (recur out (first replacements) (rest replacements))))))

(def smallest (atom 0))

(def found-list (atom []))

(defn add-to-found-list [found]
  (let [length (count found)]
    (if (or (= 0 @smallest) (< length @smallest))
      (reset! smallest length)
      (println "found:" length)))
  (swap! found-list conj found))

(def terminal-set (atom #{}))

(defn add-to-terminal-set [s]
  (swap! terminal-set conj s))

(defn terminal? [s]
  (set/subset? #{s} @terminal-set))

(defn reducer [start replacements history]
  (let [reductions (make-reductions start replacements)]
    (if-not (empty? reductions)
      (do
        (doseq [r reductions]
          (if (= r "e")
            (add-to-found-list (conj history r))
            (when-not (terminal? r)
              (reducer r replacements (conj history r))))))
      (add-to-terminal-set start))))

(defn compare-replacements [x y]
  (let [x0 (get x 0)
        x1 (get x 1)
        y0 (get y 0)
        y1 (get y 1)
        count-x0 (count x0)
        count-x1 (count x1)
        count-y0 (count y0)
        count-y1 (count y1)]
    (if (> count-x0 count-y0)
      true
      (if (= count-x0 count-y0)
        (if (> count-y1 count-x1)
          true
          (if (= count-x1 count-y1)
            (> (.compareTo x1 y1) 0)))))))

(defn sort-replacements [replacements]
  (sort (comparator compare-replacements) replacements))

(defn line->replacement [line]
  (let [tokes (str/split line #" ")]
    (when (= "=>" (get tokes 1))
      [(get tokes 0) (get tokes 2)])))

(defn lines->replacements [lines]
  (loop [out []
          l (first lines)
          lines (rest lines)]
    (if-not l
      out
      (let [replacement (line->replacement l)]
        (if replacement
          (recur (conj out replacement) (first lines) (rest lines))
          (recur out (first lines) (rest lines)))))))

(defn read-file [file]
  (let [input (slurp file)
        lines (str/split input #"\n")]
    {:replacements (lines->replacements lines)
      :start (last lines)}))

(defn replace-file [file]
  (let [{:keys [:replacements :start]} (read-file file)]
    (replacer start replacements)))

(defn count-new-molecules [file]
  (count (replace-file file)))

(defn reduce-to-e [file]
    (let [{:keys [:replacements :start]} (read-file file)]
      (reducer start (-> replacements flip-replacements sort-replacements) [])))

(defn count-reduction-steps [file]
  (reset! found-list [])
  (reset! terminal-set #{})
  (reset! reductions-atom #{})
  (reset! smallest 0)
  (reduce-to-e file)
  @smallest)


(defn run []
  (println "Day 19, part 1:" (count-new-molecules "src/advent_of_code/day19/input.txt"))
  (println "Day 19, part 2:" (count-reduction-steps "src/advent_of_code/day19/input.txt")))
