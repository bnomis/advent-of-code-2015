(ns advent-of-code.day08.core
  (:require [clojure.java.io :as io]))

(def new-line 10)
(def eof -1)
(def double-quote 34)
(def backslash 92)
(def x-char 120)

(defn read-a-line [xin]
  (loop [c (.read xin)
          out []]
    (if (or (= c new-line) (= c eof))
      out
      (recur (.read xin) (conj out c)))))

(defn read-and-put-back-char [xin]
  (.mark xin 1)
  (let [c (.read xin)]
    (.reset xin)
    c))

(defn read-lines [file]
  (with-open
    [xin (io/input-stream file)]
    (loop [lines []
            c (read-and-put-back-char xin)]
      (if (= c eof)
        lines
        (recur (conj lines (read-a-line xin)) (read-and-put-back-char xin))))))

(defn next-char [line data]
  (get line (+ 1 (:index data))))

(defn next-char-is-x [line data]
  (= x-char (next-char line data)))

(defn handle-backslash [line data]
  (if (next-char-is-x line data)
    (-> data
      (update :index #(+ 4 %))
      (update :count inc))
    (-> data
      (update :index #(+ 2 %))
      (update :count inc))))

(defn consume-char [line data]
  (let [index (:index data)
        count (:count data)
        c (get line index)]
    (if (= c backslash)
      (handle-backslash line data)
      (-> data
        (update :index inc)
        (update :count inc)))))

(defn char-count [line]
  ;;(println line)
  (let [length (count line)
        ;; starts and ends with double quote
        line (subvec line 1 (- length 1))
        length (- length 2)]
    ;;(println line)
    (loop [data {:count 0 :index 0}]
      (if (> (:index data) (- length 1))
        (do
          ;;(println data)
          (:count data))
        (recur (consume-char line data))))))

(defn is-special [c]
  (condp = c
    double-quote true
    backslash true
    false))

(defn encode [encoded c]
  (if (is-special c)
    (do
      (-> encoded
        (conj backslash)
        (conj c)))
    (do
      (conj encoded c))))

(defn encoded-count [line]
  (loop [c (first line)
          line (rest line)
          encoded []]
    (if-not c
      (do
        ;;(println encoded)
        (+ 2 (count encoded)))
      (recur (first line) (rest line) (encode encoded c)))))

(defn parse-line [line]
  (- (count line) (char-count line)))

(defn parse-lines [lines]
  (mapv parse-line lines))

(defn parse-line-2 [line]
  (- (encoded-count line) (count line)))

(defn parse-lines-2 [lines]
  (mapv parse-line-2 lines))

(defn reduce-lines [lines]
  (reduce + lines))

(defn parse-file [file]
  (-> file
    (read-lines)
    (parse-lines)
    (reduce-lines)))

(defn parse-file-2 [file]
  (-> file
    (read-lines)
    (parse-lines-2)
    (reduce-lines)))

(defn run []
  (println "Day 08, part 1" (parse-file "src/advent_of_code/day08/input.txt"))
  (println "Day 08, part 2" (parse-file-2 "src/advent_of_code/day08/input.txt")))
