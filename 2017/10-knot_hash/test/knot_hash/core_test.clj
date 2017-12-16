(ns knot-hash.core-test
  (:require [clojure.test :refer :all]
            [knot-hash.core :refer :all]))

(deftest step-should-set-next
  (testing "first step in example")
  (is (= '(2 1 0 3 4) (step '(0 1 2 3 4) '(3 4 1 5)))))
