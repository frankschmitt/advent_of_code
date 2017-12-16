(ns knot-hash.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn step
  "Given the current state and the list of input lengths, compute the next step"
  [state, lengths]
  (let [offset (first lengths)]
  (concat (reverse (take offset state)) (drop offset state))))
