(ns ^{:doc "This namespace contains generic functions to manipulate arbitrary
      data structures. It knows nothing about the game rules."}
    domi.internal.misc)


(defn highest [m min max]
  "Returns the entry from the map m with the highest key within the range
  [min, max]."
  (->> m
       (filter
         (fn [[k _]] (<= min k max)))
       (sort-by first)
       last))


(defn sum-by [attr coll]
  "Sums up the attr values in coll."
  (reduce + (map attr coll)))


(defn vec-set [x]
  "If x is a vector, makes a set out of it.
  Otherwise, makes a set out of a vector of x."
  (if (vector? x)
    (set x)
    (set (vector x))))
