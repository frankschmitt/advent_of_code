(ns knot-hash.core
  (:gen-class))

(defrecord KnotHashState [input, position, skip, lengths])

;; Example:
;;(assoc-multiple [1 2 3 4 5 6 7 8] [0 1] [10 20])
;;   => [10 20 3 4 5 6 7 8]
(defn assoc-multiple
  "Replaces multiple values in a vector (given by index and value lists)"
  [v indices values]
  (->> (interleave indices values)
       (apply assoc v)))


(defn hash-step
  "Given input, starting pos and length, reverse the substring starting at pos and having the given length (with wrap-around"
  [old-state]
  (let [ old-input (:input old-state)
         wrapped-input (cycle old-input)
         overall_length (count old-input)
         seg1_length (:position old-state)
         seg2_length (first (:lengths old-state))
         ;seg3_length (- overall_length seg1_length seg2_length)
         ;seg1 (take seg1_length old-input)
         seg2 (reverse (take seg2_length (drop seg1_length wrapped-input)))
         ;seg3 (take seg3_length (drop (+ seg1_length seg2_length) wrapped-input))
         ;; indices from seg1_length+1 .. seg1_length+seg2_length, modulo overall_length (= segment we want to replace)
         indices-to-switch (map #(mod (+ seg1_length (- %1 1)) overall_length) (range 1 (inc seg2_length)))
        ]
        ;(concat seg1 seg2 seg3)))
        (assoc-multiple old-input indices-to-switch seg2)))

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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
