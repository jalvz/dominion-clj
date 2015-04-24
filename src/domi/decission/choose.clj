(ns ^{:doc "This namespace decides what cards to buy or play."}
    domi.decission.choose
  (:use (domi.internal [misc :only (highest)]
                       [search :only (filter-from-deck is?)])))



(defn choose-action [player]
  "Returns an action from player's hand to play"
  (when-let [actions-in-hand (filter-from-deck player :kingdom :hand)]
    (first actions-in-hand)))




(defn- internal-choose-buy [money lower-limit supply filter-fn]
  "Returns the card from the supply to buy which price is no less than
  lower-limit."
  (let [buyables (map first (vals @supply))
        cost-view (group-by :cost (filter-fn buyables))]
    (when-let [available-buys
               (highest cost-view lower-limit money)]
      (:name
        (rand-nth
          (val available-buys))))))

(defn choose-buy
  ([upper-limit lower-limit supply]
    (choose-buy upper-limit lower-limit supply [:treasure :kingdom :victory]))
  ([upper-limit lower-limit supply type]
    (internal-choose-buy upper-limit
                         lower-limit
                         supply
                         (partial
                           filter
                           #(is? type %)))))

