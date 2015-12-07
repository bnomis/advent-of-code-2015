(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day01.core :refer [find-floor]]
            [advent-of-code.day02.core :refer [find-area find-ribbon]]
            [advent-of-code.day03.core :refer [find-houses find-houses-robo]]
            [advent-of-code.day04.core :refer [find-number make-hash starts-with-five-zeros]]
            [advent-of-code.day05.core :refer [nice-string nice-string-2]]
            [advent-of-code.day06.core :refer [run-instructions]]))

(deftest day-01
  (testing "Floor."
    (is (= 1 (find-floor (list \())))
    (is (= -1 (find-floor (list \)))))
    (is (= 2 (find-floor (list \( \())))
    (is (= 1 (find-floor (list \( \( \)))))
    (is (= 0 (find-floor (list \( \( \) \)))))
    (is (= -1 (find-floor (list \( \( \) \) \)))))))

(deftest day-02
  (testing "Area."
    (is (= 58 (find-area "2x3x4")))
    (is (= 43 (find-area "1x1x10")))))

(deftest day-02-2
  (testing "Ribbon."
    (is (= 34 (find-ribbon "2x3x4")))
    (is (= 14 (find-ribbon "1x1x10")))))

(deftest day-03
  (testing "Houses."
    (is (= 2 (count (find-houses (list \>)))))
    (is (= 4 (count (find-houses (list \^ \> \v \<)))))
    (is (= 2 (count (find-houses (list \^ \v \^ \v \^ \v \^ \v \^ \v)))))))

(deftest day-03-2
  (testing "Houses with robo santa."
    (is (= 3 (count (find-houses-robo (list \^ \v)))))
    (is (= 3 (count (find-houses-robo (list \^ \> \v \<)))))
    (is (= 11 (count (find-houses-robo (list \^ \v \^ \v \^ \v \^ \v \^ \v)))))))

(deftest day-04-hash
  (testing "Hash identifier"
    (is (= true (starts-with-five-zeros (make-hash "abcdef609043"))))
    (is (= true (starts-with-five-zeros (make-hash "pqrstuv1048970"))))))

(deftest day-04
  (testing "Hashes"
    (is (= 609043 (find-number "abcdef")))
    (is (= 1048970 (find-number "pqrstuv")))))

(deftest day-05
  (testing "Nice strings"
    (is (= true (nice-string "ugknbfddgicrmopn")))
    (is (= true (nice-string "aaa")))
    (is (= false (nice-string "jchzalrnumimnmhp")))
    (is (= false (nice-string "haegwjzuvuyypxyu")))
    (is (= false (nice-string "dvszwmarrgswjxmb")))))

(deftest day-05-2
  (testing "Super nice strings"
    (is (= true (nice-string-2 "qjhvhtzxzqqjkmpb")))
    (is (= true (nice-string-2 "xxyxx")))
    (is (= false (nice-string-2 "uurcxstgmygtbstg")))
    (is (= false (nice-string-2 "ieodomkazucvgmuy")))
    (is (= false (nice-string-2 "dieatyxxxlvhneoj")))))


(deftest day-06
  (testing "Illuminations"
    (is (= 9 (run-instructions 3 3 ["turn on 0,0 through 2,2"])))
    (is (= 4 (run-instructions 3 3 ["turn on 0,0 through 1,1"])))
    (is (= 5 (run-instructions 3 3 ["turn on 0,0 through 2,2" "turn off 0,0 through 1,1"])))
    (is (= 4 (run-instructions 3 3 ["turn on 0,0 through 2,2" "turn off 0,0 through 1,1" "toggle 0,0 through 2,2"])))))
