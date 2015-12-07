(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day01.core :refer [find-floor]]
            [advent-of-code.day02.core :refer [find-area]]
            [advent-of-code.day03.core :refer [find-houses]]
            [advent-of-code.day04.core :refer [find-number make-hash starts-with-five-zeros]]
            [advent-of-code.day05.core :refer [nice-string]]))

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

(deftest day-03
  (testing "Houses."
    (is (= 2 (find-houses (list \>))))
    (is (= 4 (find-houses (list \^ \> \v \<))))
    (is (= 2 (find-houses (list \^ \v \^ \v \^ \v \^ \v \^ \v))))))

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
