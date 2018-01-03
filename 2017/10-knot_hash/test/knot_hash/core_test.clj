(ns knot-hash.core-test
  (:require [clojure.test :refer :all]
            [knot-hash.core :refer :all]))

(deftest hash-step-first-iteration
  (testing "hashing, first step in example")
  (let [input [0 1 2 3 4]
        lengths '(3 4 1 5)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        ;; reverse first 3 elements, inc pos by 3, inc skip by 1, pop lengths
        expected [2 1 0 3 4]
        ]
    (is (= expected (hash-step start-state)))))

(deftest hash-step-second-iteration
  (testing "hashing, second step in example"
    (let [input [2 1 0 3 4]
          lengths '(4)
          pos 3
          skip 1
          start-state (knot-hash.core/->KnotHashState input pos skip lengths)
          expected [4 3 0 1 2]
          ]
      (is (= expected (hash-step start-state))))))

(deftest step-should-work-for-no-wrap
  (testing "first step in example")
  (let [input [0 1 2 3 4]
        lengths '(3 4 1 5)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        ;; reverse first 3 elements, inc pos by 3, inc skip by 1, pop lengths
        expected-state (knot-hash.core/->KnotHashState [2 1 0 3 4] 3 1  '(4 1 5))
        ]
    (is (= expected-state (step start-state)))))

; The second length, 4, selects a section which wraps: 2 1) 0 ([3] 4.
; The sublist 3 4 2 1 is reversed to form 1 2 4 3: 4 3) 0 ([1] 2.
; The current position moves forward by the length plus the skip size, a total of 5, causing it not to move because it wraps around: 4 3 0 [1] 2
; The skip size increases to 2.
(deftest step-should-work-with-wrap
  (testing "second step in example")
  (let [input [2 1 0 3 4]
        lengths '(4 1 5)
        pos 3
        skip 1
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        ;; reverse 3 4 2 1, inc pos by 3+2, inc skip by 1, pop lengths
        expected-state (knot-hash.core/->KnotHashState [4 3 0 1 2] 3 2  '(1 5))
        ]
    (is (= expected-state (step start-state)))))

(deftest solve-should-return-correct-solution-for-sample
  (testing "solve should return the correct solution for the sample input")
  (let [input [0 1 2 3 4]
        lengths '(3 4 1 5)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        expected-state (knot-hash.core/->KnotHashState [3 4 2 1 0] 4 4  '())
        ]
    (is (= expected-state (solve start-state)))))

(deftest solve-should-solve-part-I
  (testing "solve should return the correct solution for part I")
  (let [input (into [] (range 0 256))
        lengths '(197 97 204 108 1 29 5 71 0 50 2 255 248 78 254 63)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        end-state (solve start-state)
        num1 (first (:input end-state))
        num2 (first (rest (:input end-state)))
        ]
    (is (= 40132 (* num1 num2)))))

(deftest input-to-lengths-should-use-ascii-codes-and-append-default-lengths
  (testing "input-to-lengths should convert the input string to ASCII codes and append the default lengths"
    (is (= '(49 44 50 17 31 73 47 23) (input-to-lengths "1,2")))
    (is (= '(49 44 50 44 51 17 31 73 47 23) (input-to-lengths "1,2,3")))
    )
  )

(deftest xor-for-block-should-return-correct-value-for-example
  (testing "xor-for-block should return correct value for the example"
    (is (= 64 (xor-for-block '(65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22))))
    )
  )

(deftest xor-for-block-should-return-correct-value-for-two-values
  (testing "xor-for-block should return correct value for two values"
    (is (= 128 (xor-for-block '(0 128))))
    )
  )