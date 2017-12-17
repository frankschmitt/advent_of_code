(ns knot-hash.core
  (:gen-class))

(defrecord KnotHashState [input, position, skip, lengths])

;;(defrecord Person [fname lname address])

(defn step
  "Given the current state and the list of input lengths, compute the next step"
  [old-state]
  (let [offset (first (:lengths old-state))
        ;old-input (cycle (:input old-state)) ; use cycle to wrap around
        old-input (:input old-state) ; use cycle to wrap around
        old-pos (:position old-state)
        length (count old-input)
        ; construct new input
        ;seg1 (take old-pos old-input) ; first part: unchanged
        ;seg2 (take offset (drop old-pos old-input)) ; second part: reversed
        ;seg3 (take length (drop (+ offset old-pos) old-input)) ; third part: unchanged (too long, but we'll fix that in the next step)
        ;new-input (take length (concat seg1 (reverse seg2) seg3))

        ;new-input (concat (take offset old-input) (reverse (drop offset old-input)))
        new-input (concat (reverse (take offset old-input)) (drop offset old-input))

        new-pos (mod (+ old-pos offset) length)
        new-skip (+ 1 (:skip old-state))
        new-lengths (rest (:lengths old-state))
        ]
    (->KnotHashState new-input new-pos new-skip new-lengths)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
