(ns had-utils.collections)

(defn map-map
  "Construct a new map from an existing one.
  Each of val-fn and (optional - default (fn [k v] k)) key-fn are
  a function of two arguments, the key and value.
  Note that to make the usage more natural the optional key-fn
  argument goes in the second place when used."
  ([m val-fn]
   (map-map m identity val-fn))
  ([m key-fn val-fn]
   (reduce-kv #(assoc %1 (key-fn %2 %3) (val-fn %2 %3)) {} m)))

(defn count-when
  "Count the number of elements in coll where f returns true. If not supplied use identity as f."
  ([coll] (count-when coll identity))
  ([coll f] (count (filter f coll))))

(defn transpose [seqs]
  "Transpose a rectangular sequence of sequences."
  (loop [xs seqs acc []]
    (if (empty? (first xs))
      acc
      (recur (map rest xs) (conj acc (map first xs))))))

