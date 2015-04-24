(ns test.test-helper
  (:use [domi.cards.card])
  (:require [domi.cards.card :as cards]))


(defn- pcount [deck player]
  (count (deck @player)))

(def hcount (partial pcount :hand))

(def kcount (partial pcount :player-deck))

(def dcount (partial pcount :discard))

(def tcount (partial pcount :trash))

(def icount (partial pcount :in-play))

(defn ocount [player]
  (apply + ((juxt hcount kcount dcount tcount icount) player)))


(defn init-card [card-name]
  "Created a card from the name, with the in-game metadata attached to it."
  (first (build-deck {card-name 1})))


(defn add!
  "Adds a card to the player's hand, unless other deck is specified."
  ([player card-name] (add! player card-name :hand))
  ([player card-name deck]
    (dosync
      (alter player update-in [deck] conj (init-card card-name))
      player)))


(defn addc!
  "Adds a card to the player's hand, unless other deck is specified."
  ([player card] (add! player card :hand))
  ([player card deck]
    (dosync
      (alter player update-in [deck] conj card))))


(defn add-all!
  "Adds one copy of each card to the player's hand, unless other deck is
  specified."
  ([player] (add-all! player :hand))
  ([player deck] (add-all! player deck cards/names))
  ([player deck card-names]
    (dosync
      (alter player update-in [deck] concat (map init-card card-names))
      player)))



(defn put!
  "Puts cards in player's hand, unless other deck is specified.
  card-map maps card names to number of copies.
  It removes the cards in the deck that the player had before."
  ([player card-map] (put! player card-map :hand))
  ([player card-map deck]
    (dosync
      (alter player assoc-in [deck] (build-deck card-map))
      player)))


(defn put1!
  "Puts one copy of each of the cards in the player's hand, unless other deck
  is specified.
  It removes the cards in the deck that the player had before."
  ([player card-names] (put1! player card-names :hand))
  ([player card-names deck] (put! player (reduce
                                           #(into %1 {%2 1}) {} card-names)
                                  deck)))


(defn put1-all!
  "Puts one copy of each Dominion card in the player's hand, unless other deck
  is specified.
  It removes the cards in the deck that the player had before."
  ([player] (put1! player cards/names))
  ([player deck] (put1! player cards/names deck)))