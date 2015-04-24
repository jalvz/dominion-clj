(ns ^{:doc "This namespace contains functions to transform and query turn
      resources."}
    domi.internal.turn)


(defn available? [resources type]
  "Checks if there are resources of specified type."
  (pos? (type resources)))


(defn gain [resources transform]
  "It returns a new resources records with transform applied, where transform
  is a map of resource attributes to deltas."
  (let [update-resources-fn
        (fn [acc [type_ delta]]
          (update-in acc [type_] + delta))]
    (reduce
      update-resources-fn
      resources
      transform)))


(defn consume [resources attr]
  "Consumes one resource of one type."
  {:pre (pos? (attr resources))}
  (update-in resources [attr] - 1))