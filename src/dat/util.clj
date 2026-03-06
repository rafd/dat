(ns dat.util)

(defn remove-nil-vals
  [m]
  (->> m
       (filter val)
       (into {})))
