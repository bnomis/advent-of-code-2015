(ns advent-of-code.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code.day01.core :refer [find-floor]]
            [advent-of-code.day02.core :refer [find-area]]))

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
