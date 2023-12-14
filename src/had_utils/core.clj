(ns had-utils.core)

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

(defn find-cycle
  "Apply `update-fn` to `input` iteratively until a cycle is found.
  Return [`start` `end`] of the cycle, so applying `start` times is the
  same as `end` times. An optional `hash-fn` may be provided - in this
  case when two results have the same hash the cycle is reported."
  ([input update-fn] (find-cycle input update-fn identity))
  ([input update-fn hash-fn]
   (loop [seen {(hash-fn input) 0}
          n 1
          input input]
     (let [new-input (update-fn input)
           new-key (hash-fn new-input)]
       (if-let [n-found (get seen new-key)]
         [n-found n]
         (recur (assoc seen new-key n)
                (inc n)
                new-input))))))

(defn find-cyclic-equivalent
  "See also `find-cycle`. Assuming applying `update-fn` repeatedly to `input` produces a cycle
  (or repeats a hash if the optional `hash-fn` is provided), finds an equivalent number of
  applications of `update-fn` to the provided `n` but less than the number needed to reach the
  end of the first cycle."
  ([input n update-fn] (find-cyclic-equivalent input n update-fn identity))
  ([input n update-fn hash-fn]
   (let [[s e] (find-cycle input update-fn hash-fn)
         cycle-length (- e s)]
     (if (<= n s)
       n
       (+ s (mod (- n s) cycle-length))))))

(defn find-cyclic
  "See also `find-cycle`. Finds the result of applying `update-fn` to `input` `n`
  times by first detecting a cycle in the iteration using `find-cyclic-equivalent`
  and then computing based on that. Note that there is an inherent inefficiency
  because we compute first to find the cycle and then again to find the answer.
  This is necessary to support non-identity `hash-fn`."
  ([input n update-fn] (find-cyclic input n update-fn identity))
  ([input n update-fn hash-fn]
   (let [n (find-cyclic-equivalent input n update-fn hash-fn)]
     (first (drop n (iterate update-fn input))))))
