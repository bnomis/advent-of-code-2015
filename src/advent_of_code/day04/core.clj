(ns advent-of-code.day04.core
  (:import (java.security MessageDigest)))

(defn starts-with-five-zeros [number]
  (if (= "00000" (apply str (take 5 (seq number))))
    true
    false))

(defn starts-with-six-zeros [number]
  (if (= "000000" (apply str (take 6 (seq number))))
    true
    false))

(defn make-hex [input]
  (apply str (map (partial format "%02x") input)))

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
