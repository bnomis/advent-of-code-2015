(defproject advent-of-code "1.0.0"
  :description "Advent of Code 2015"
  :url "https://github.com/bnomis/advent-of-code-2015"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}

  :dependencies [
                  [org.clojure/clojure "1.7.0"]
                  [org.clojure/data.json "0.2.6"]
                  [org.clojure/math.combinatorics "0.1.1"]
                  [org.clojure/core.logic "0.8.10"]]

  :main ^:skip-aot advent-of-code.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
