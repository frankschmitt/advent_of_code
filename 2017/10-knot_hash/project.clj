(defproject knot-hash "0.1.0-SNAPSHOT"
  :description "Solution for the knot hash Advent of Code challenge 2017"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot knot-hash.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
