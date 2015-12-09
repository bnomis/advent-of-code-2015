(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day01.core :refer [find-floor]]
            [advent-of-code.day02.core :refer [find-area find-ribbon]]
            [advent-of-code.day03.core :refer [find-houses find-houses-robo]]
            [advent-of-code.day04.core :refer [find-number make-hash starts-with-five-zeros]]
            [advent-of-code.day05.core :refer [nice-string nice-string-2]]
            [advent-of-code.day06.core :refer [run-instructions]]
            [advent-of-code.day07.core :refer [run-circuit]]
            [advent-of-code.day08.core :refer [parse-file parse-file-2]]))

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

(deftest day-07
  (testing "Circuits"
    (is (= {:x 123} (run-circuit ["123 -> x"])))
    (is (= {:x 123 :y 456} (run-circuit ["123 -> x" "456 -> y"])))
    (is (= {:d 72 :x 123 :y 456} (run-circuit ["123 -> x" "456 -> y" "x AND y -> d"])))
    (is (= {:d 72 :e 507 :x 123 :y 456} (run-circuit ["123 -> x" "456 -> y" "x AND y -> d" "x OR y -> e"])))
    (is (= {:d 72 :e 507 :f 492 :x 123 :y 456} (run-circuit ["123 -> x" "456 -> y" "x AND y -> d" "x OR y -> e" "x LSHIFT 2 -> f"])))
    (is (= {:d 72 :e 507 :f 492 :g 114 :x 123 :y 456} (run-circuit ["123 -> x" "456 -> y" "x AND y -> d" "x OR y -> e" "x LSHIFT 2 -> f" "y RSHIFT 2 -> g"])))
    (is (= {:d 72 :e 507 :f 492 :g 114 :h 65412 :x 123 :y 456} (run-circuit ["123 -> x" "456 -> y" "x AND y -> d" "x OR y -> e" "x LSHIFT 2 -> f" "y RSHIFT 2 -> g" "NOT x -> h"])))
    (is (= {:d 72 :e 507 :f 492 :g 114 :h 65412 :i 65079 :x 123 :y 456} (run-circuit ["123 -> x" "456 -> y" "x AND y -> d" "x OR y -> e" "x LSHIFT 2 -> f" "y RSHIFT 2 -> g" "NOT x -> h" "NOT y -> i"])))
    (is (= {:cc 123 :cd 1} (run-circuit ["123 -> cc" "1 AND cc -> cd"])))))

(deftest day-08
  (testing "Reading"
    (is (= 2 (parse-file "src/advent_of_code/day08/input-test-01.txt")))
    (is (= 2 (parse-file "src/advent_of_code/day08/input-test-02.txt")))
    (is (= 3 (parse-file "src/advent_of_code/day08/input-test-03.txt")))
    (is (= 5 (parse-file "src/advent_of_code/day08/input-test-04.txt")))
    (is (= 9 (parse-file "src/advent_of_code/day08/input-test-05.txt")))
    (is (= 4 (parse-file-2 "src/advent_of_code/day08/input-test-01.txt")))
    (is (= 4 (parse-file-2 "src/advent_of_code/day08/input-test-02.txt")))
    (is (= 6 (parse-file-2 "src/advent_of_code/day08/input-test-03.txt")))
    (is (= 5 (parse-file-2 "src/advent_of_code/day08/input-test-04.txt")))
    (is (= 8 (parse-file-2 "src/advent_of_code/day08/input-test-05.txt")))))
