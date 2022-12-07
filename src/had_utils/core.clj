(ns had-utils.core)

(defn step-state-on-input [initial-state step-f input-seq]
  "Take an initial state initial-state, a function that steps
the state based on pieces of input step-f and a sequence
of pieces of input. Step the state over all of the input."
  (loop [state initial-state
         input input-seq]
    (if (empty? input)
      state
      (recur (step-f state (first input)) (rest input)))))


(defn iterate-until [f x pred]
  "Iterate one argument function f on starting value x
and return the first iterate that makes pred true."
  (->> (iterate f x)
       (filter pred)
       first))

(defn reverse-args [f]
  "Given a function returns a new function that takes its
arguments in the reverse order."
  (fn [& args] (apply f (reverse args))))

(defn flip-args [f]
  "Given a function that takes at least two arguments
returns a new function that reverses the order of
the first two arguments"
  (fn [x y & rest]
    (apply f y x rest)))
