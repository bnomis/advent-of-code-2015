(ns advent-of-code.day15.logic
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require
    [clojure.core.logic.fd :as fd]))


(defn make-values-2 []
  (run* [q]
    (fresh [a b]
      (fd/in a b (fd/interval 0 100))
      (fd/eq (= (+ a b) 100))
      (== q [a b]))))

(defn make-values-4 []
  (run* [q]
    (fresh [a b c d]
      (fd/in a b c d (fd/interval 0 100))
      (fd/eq (= (+ a b c d) 100))
      (== q [a b c d]))))

(defn make-values [vars]
  (case vars
    2 (make-values-2)
    4 (make-values-4)
    nil))
