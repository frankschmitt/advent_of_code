(ns knot-hash.core
  (:gen-class))

(defrecord KnotHashState [input, position, skip, lengths])

(defn hash-step
  "Given input, starting pos and length, reverse the substring starting at pos and having the given length (with wrap-around"
  [old-state]
  (let [ old-input (:input old-state)
         ;wrapped-input (wrap-input old-state)
         wrapped-input (cycle old-input)
         overall_length (count old-input)
         seg1_length (:position old-state)
         seg2_length (first (:lengths old-state))
         seg3_length (- overall_length seg1_length seg2_length)
         seg1 (take seg1_length old-input)
         seg2 (reverse (take seg2_length (drop seg1_length wrapped-input)))
         seg3 (take seg3_length (drop (+ seg1_length seg2_length) wrapped-input))
        ]
        ;(take length (concat (reverse (take offset wrapped-input)) (drop offset wrapped-input)))))
        (concat seg1 seg2 seg3)))

(defn step
  "Given the current state and the list of input lengths, compute the next step"
  [old-state]
  (let [offset (first (:lengths old-state))
        old-input (:input old-state)
        old-skip (:skip old-state)
        length (count old-input)
        old-pos (:position old-state)
        wrapped-input (drop old-pos (take (* 3 length) (cycle old-input))) ; hopefully, wrapping around 3 times should be enough
        ;wrapped-input (wrap-input old-state)
        ; construct new input
        seg1 (take old-pos wrapped-input) ; first part: unchanged
        seg2 (reverse (take offset (drop old-pos wrapped-input))) ; second part: reversed
        seg3 (take length (drop (+ offset old-pos) wrapped-input)) ; third part: unchanged (too long, but we'll fix that in the next step)
        new-input (take length (concat seg1 seg2 seg3))

        ; works for simple cases
        ; new-input (take length (concat (reverse (take offset wrapped-input)) (drop offset wrapped-input)))

        new-pos (mod (+ old-pos offset old-skip) length)
        ; our new input is now shifted; we need to shift it back
        ;wrapped-new-input (take length (drop (- length new-pos) (cycle new-input)))
        new-skip (+ 1 old-skip)
        new-lengths (rest (:lengths old-state))
        ]
    (->KnotHashState new-input new-pos new-skip new-lengths)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
