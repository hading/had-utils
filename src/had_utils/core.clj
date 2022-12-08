(ns had-utils.core)

(defn step-state-on-input
  "Take an `initial-state`, a function `step-f` that
  produces a new state based on a current state and
  piece of input, and a sequence of input `input-seq`
  and return the final state after stepping over all
  the input."
  [initial-state step-f input-seq]
  (loop [state initial-state
         input input-seq]
    (if (empty? input)
      state
      (recur (step-f state (first input)) (rest input)))))

(defn update-vec-state [f initial-v]
  "Take the initial vector `init-v` and the function
`f` of vec and index, and successively update state by
applying `f` to the current state and indexes.
Basically a reduce over the state where you can use the
index."
  (reduce f initial-v (range (count initial-v))))

(defn iterate-until
  "Iterate one argument function `f` on starting value `x`
  and return the first iterate that makes `pred` true."
  [f x pred]
  (->> (iterate f x)
       (filter pred)
       first))

(defn reverse-args
  "Given a function `f` returns a new function that takes its
arguments in the reverse order."
  [f]
  (fn [& args] (apply f (reverse args))))

(defn flip-args
  "Given a function `f` that takes at least two arguments
returns a new function that reverses the order of
the first two arguments"
  [f]
  (fn [x y & rest]
    (apply f y x rest)))
