(ns knot-hash.core
  (:gen-class))

(defrecord KnotHashState [input, position, skip, lengths])

;; Example (replaces the elements at index 0 and 1 with 10 and 20):
;;(assoc-multiple [1 2 3 4 5 6 7 8] [0 1] [10 20])
;;   => [10 20 3 4 5 6 7 8]
(defn assoc-multiple
  "Replaces multiple values in a vector (given by index and value lists)"
  [v indices values]
  ;; ensure the number of indices and values match
  (assert (= (count indices) (count values)))
  ;; if indices and values are empty: return the original vector, otherwise: repeatedly apply assoc to replace one element
  (if (= 0 (count indices))
    v
    (->> (interleave indices values)
         (apply assoc v)))
  )

;; slice a list into equally-sized chunks
(defn slice
  "Slices a list into equally-sized chunks"
  [chunk-size v]
  (let [tmp (split-at chunk-size v)
        head (first tmp)
        tail (first (rest tmp))]
    (if (empty? tail)
      (cons head ()) ;; stupid HACK to get a 1-element list containing head (neither (head) nor '(head) does what I want)
      (cons head (slice chunk-size tail))))
  )

;; convert an input string to a list of lengths
(defn input-to-lengths
  "Converts each character in the input to its ASCII code to obtain the user-defined lengths, and append the default lengths"
  [str]
  (let [ascii (map int str)
        default-lengths '(17 31 73 47 23)]
    (concat ascii default-lengths)
    )
  )

;; compute the XOR for a list of input values
(defn xor-for-block
  "Compute the XOR for the given inputs"
  [input]
  (reduce bit-xor input)
  )

;; convert a sparse hash into a dense hash (by "folding" it)
(defn condense-hash
  "Condense a sparse hash to a dense hash by XOR-ing groups of 16"
  [input]
  (let [chunks (slice 16 input)]
    (map xor-for-block chunks))
  )

;; convert a list of numbers to the corresponding hex string
(defn list-to-hex-string
  "Convert a list of numbers to the corresponding hex string"
  [input]
  (apply str (map #(format "%02x" %1) input))
  )

;; perform a single hash step (input part only)
(defn hash-step
  "Given input, starting pos and length, reverse the substring starting at pos and having the given length (with wrap-around"
  [old-state]
  (let [ old-input (:input old-state)
         wrapped-input (cycle old-input)
         overall-length (count old-input)
         seg1-length (:position old-state)
         seg2-length (first (:lengths old-state))
         seg2 (reverse (take seg2-length (drop seg1-length wrapped-input)))
         ;; indices from seg1_length+1 .. seg1_length+seg2_length, modulo overall_length (= segment we want to replace)
         indices-to-switch (map #(mod (+ seg1-length %1) overall-length) (range 0 seg2-length))
        ]
        (assoc-multiple old-input indices-to-switch seg2)))

;; perform a single step in the algorithm (computing new input, position and skip)
(defn step
  "Given the current state and the list of input lengths, compute the next step"
  [old-state]
  (let [offset (first (:lengths old-state))
        old-input (:input old-state)
        old-skip (:skip old-state)
        length (count old-input)
        old-pos (:position old-state)

        ;; construct new input
        new-input (hash-step old-state)
        ;; compute new position
        new-pos (mod (+ old-pos offset old-skip) length)
        ;; increase skip, and remove head element from list of lengths
        new-skip (+ 1 old-skip)
        new-lengths (rest (:lengths old-state))
        ]
    (->KnotHashState new-input new-pos new-skip new-lengths)))

(defn print-state
  "Prints a Knot Hash state to STDOUT"
  [msg state]
  (println(format "%s, pos: %d, skip: %d" msg (:position state) (:skip state)))
  (println (concat "    , lengths: " (:lengths state) ", input: " (:input state)))
  )

;; solve part I
(defn solve
  "Given the input state, compute the final state"
  [input-state]
  ;(print-state "start of solve, input: " input-state)
  (loop [new-state (step input-state)]
    (if (empty? (:lengths new-state))
      new-state
      (recur (step new-state)) )
    )
  )

(defn compute-hash-part-II
  "Given an input string, compute the hash according to part II"
  [str]
  (let [input (into [] (range 0 256))
        pos 0
        skip 0
        lengths (input-to-lengths str)
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        count 0
        ]
    (loop [new-count (inc count)
           tmp-state (solve start-state)]
      (let [next-state (knot-hash.core/->KnotHashState (:input tmp-state) (:position tmp-state) (:skip tmp-state) lengths)
            ]
        ;(println (format "count: %d, hash: %s" new-count (list-to-hex-string (condense-hash (:input tmp-state)))))
        (if (= new-count 64)
          tmp-state
          (recur (inc new-count) (solve next-state))
          ))))
  )

;; solve part II
(defn solve2
  "Given an input string, treat it as a list of lengths, and compute the final state"
  [str]
  (let [end-state (compute-hash-part-II str)]
        (list-to-hex-string (condense-hash (:input end-state)))
    )
  )

(defn -main
  "solve it"
  [& args]
  (solve2 "")
  (let [input (into [] (range 0 256))
        lengths '(197 97 204 108 1 29 5 71 0 50 2 255 248 78 254 63)
        pos 0
        skip 0
        start-state (knot-hash.core/->KnotHashState input pos skip lengths)
        expected-state (knot-hash.core/->KnotHashState [3 4 2 1 0] 4 4 '())
        ;expected-state '(1)
        end-state (solve start-state)
        num1 (first (:input end-state))
        num2 (first (rest (:input end-state)))
        ]
    (println (* num1 num2))
    ;(print-state "end state: " end-state)
    ))