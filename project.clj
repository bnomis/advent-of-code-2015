(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [
                  [org.clojure/clojure "1.7.0"]
                  [org.clojure/data.json "0.2.6"]
                  [org.clojure/math.combinatorics "0.1.1"]
                  [org.clojure/core.logic "0.8.10"]]
                  
  :main ^:skip-aot advent-of-code.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
