(ns advent-of-code.day04.core
  (:import (java.security MessageDigest)))

(defn starts-with-five-zeros [number]
  (if (= "00000" (apply str (take 5 (seq number))))
    true
    false))

(defn make-hex [input]
  ;;(println "make-hex" input)
  (apply str (map (partial format "%02x") input)))

(defn make-md5 [input]
  (let [md (MessageDigest/getInstance "MD5")
        bytes (.getBytes input "UTF-8")]
    ;;(println "make-md5: bytes" bytes)
    (.digest md bytes)))

(defn make-hash [input]
  (let [h (make-hex (make-md5 input))]
    ;;(println "make-hash:" input "->" h)
    h))

(defn find-number [input]
  (loop [number 1]
    (let [joined (str input number)
          hashed (make-hash joined)]
      ;;(println input number joined hashed)
      (if (starts-with-five-zeros hashed)
        number
        (recur (inc number))))))

(defn run []
  (println "Day 04:" (find-number "yzbqklnj")))
