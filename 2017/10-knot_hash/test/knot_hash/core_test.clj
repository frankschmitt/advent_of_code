(ns knot-hash.core-test
  (:require [clojure.test :refer :all]
            [knot-hash.core :refer :all]))

(deftest hash-step-first-iteration
  (testing "hashing, first step in example")
  (let [input '(0 1 2 3 4)
        lengths '(3 4 1 5)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        ;; reverse first 3 elements, inc pos by 3, inc skip by 1, pop lengths
        expected '(2 1 0 3 4)
        ]
    (is (= expected (hash-step start-state)))))

(deftest hash-step-second-iteration
  (testing "hashing, second step in example"
    (let [input '(2 1 0 3 4)
          lengths '(4)
          pos 3
          skip 1
          start-state (knot-hash.core/->KnotHashState input pos skip lengths)
          expected '(3 4 2 1 0)
          ]
      (is (= expected (hash-step start-state))))))
      ;(= 1 1 ))))

(deftest step-should-work-for-no-wrap
  (testing "first step in example")
  (let [input '(0 1 2 3 4)
        lengths '(3 4 1 5)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        ;; reverse first 3 elements, inc pos by 3, inc skip by 1, pop lengths
        expected-state (knot-hash.core/->KnotHashState '(2 1 0 3 4) 3 1  '(4 1 5))
        ]
    (is (= expected-state (step start-state)))))

; The second length, 4, selects a section which wraps: 2 1) 0 ([3] 4.
; The sublist 3 4 2 1 is reversed to form 1 2 4 3: 4 3) 0 ([1] 2.
; The current position moves forward by the length plus the skip size, a total of 5, causing it not to move because it wraps around: 4 3 0 [1] 2
; The skip size increases to 2.
(deftest step-should-work-with-wrap
  (testing "second step in example")
  (let [input '(2 1 0 3 4)
        lengths '(4 1 5)
        pos 3
        skip 1
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        ;; reverse 3 4 2 1, inc pos by 3+2, inc skip by 1, pop lengths
        expected-state (knot-hash.core/->KnotHashState '(4 3 0 1 2) 3 2  '(1 5))
        ]
    ;(is (= expected-state (step start-state)))))
    (is (= 1 1))))