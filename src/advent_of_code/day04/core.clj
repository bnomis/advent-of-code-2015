(ns advent-of-code.day04.core
  (:import (java.security MessageDigest))
  (:require [clojure.string :as str]))

(defn starts-with-five-zeros [number]
  (if (= "00000" (str/join (take 5 (seq number))))
    true
    false))

(defn starts-with-six-zeros [number]
  (if (= "000000" (str/join (take 6 (seq number))))
    true
    false))

(defn make-hex [input]
  (str/join (map (partial format "%02x") input)))

(defn make-md5 [input]
  (let [md (MessageDigest/getInstance "MD5")
        bytes (.getBytes input "UTF-8")]
    (.digest md bytes)))

(defn make-hash [input]
  (let [h (make-hex (make-md5 input))]
    h))

(defn find-number [input]
  (loop [number 1]
    (let [joined (str input number)
          hashed (make-hash joined)]
      (if (starts-with-five-zeros hashed)
        number
        (recur (inc number))))))

(defn find-number-6 [input]
  (loop [number 1]
    (let [joined (str input number)
          hashed (make-hash joined)]
      (if (starts-with-six-zeros hashed)
        number
        (recur (inc number))))))

(defn run []
  (println "Day 04, part 1:" (find-number "yzbqklnj"))
  (println "Day 04, part 2:" (find-number-6 "yzbqklnj")))
