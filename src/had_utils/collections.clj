(ns had-utils.collections)

(defn map-kv
  "Construct a new map from an existing one.
  Each of val-fn and (optional - default (fn [k _] k)) key-fn are
  a function of two arguments, the key and value.
  Note that to make the usage more natural the optional key-fn
  argument goes in the second place when used."
  ([val-fn coll]
   (map-kv (fn [k _] k) val-fn coll))
  ([key-fn val-fn coll]
   (reduce-kv #(assoc %1 (key-fn %2 %3) (val-fn %2 %3)) {} coll)))

(def map-map map-kv)

(defn count-when
  "Count the number of elements in coll where f returns true. If not supplied use identity as f."
  ([coll] (count-when coll identity))
  ([coll f] (count (filter f coll))))

(defn count-by [f coll]
  "Count the number of items for each return value of f."
  (->> (group-by f coll)
       (map-kv (fn [k v] (count v)))))

(defn transpose [seqs]
  "Transpose a rectangular sequence of sequences."
  (loop [xs seqs acc []]
    (if (empty? (first xs))
      acc
      (recur (map rest xs) (conj acc (map first xs))))))

