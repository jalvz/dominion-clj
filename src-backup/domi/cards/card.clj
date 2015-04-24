(ns ^{:doc "This namespace holds the cards records"}
    domi.cards.card
  (:use [domi.internal.search :only [is?]])
  (:import (java.util UUID)))


(defrecord Card [name type cost value])


(defmethod clojure.core/print-method Card [c writer]
  "Output cards in REPL as 'x(T)', where x is the card name and T is the
  initial of the card type name"
  (.write writer (str
                   (name (:name c))
                   "("
                   (.toUpperCase (str (first (name (:type c)))))
                   ")")))


(def ^{:private true} cards
  [(->Card :copper :treasure 0 1)
   (->Card :silver :treasure 3 2)
   (->Card :gold :treasure 6 3)
   (->Card :curse :victory 0 -1)
   (->Card :estate :victory 2 1)
   (->Card :duchy :victory 5 3)
   (->Card :province :victory 8 6)
   (->Card :cellar :kingdom 2 [:speedy])
   (->Card :chapel :kingdom 2 [:trashy])
   (->Card :moat :kingdom 2 [:speedy])
   (->Card :village :kingdom 3 [:speed])
   (->Card :workshop :kingdom 3 [])
   (->Card :chancellor :kingdom 3 [])
   (->Card :woodcutter :kingdom 3 [])
   (->Card :militia :kingdom 4 [])
   (->Card :remodel :kingdom 4 [:upgradable])
   (->Card :spy :kingdom 4 [:attack])
   (->Card :thief :kingdom 4 [:attack])
   (->Card :bureucrat :kingdom 4 [])
   (->Card :feast :kingdom 4 [:upgradable])
   (->Card :gardens :kingdom 4 [:victory-ish])
   (->Card :smithy :kingdom 4 [:speedy])
   (->Card :moneylender :kingdom 4 [:trashy])
   (->Card :throne-room :kingdom 4 [])
   (->Card :council-room :kingdom 5 [])
   (->Card :festival :kingdom 5 [])
   (->Card :laboratory :kingdom 5 [:speedy])
   (->Card :library :kingdom 5 [])
   (->Card :market :kingdom 5 [:speedy])
   (->Card :mine :kingdom 5 [:upgradable])
   (->Card :witch :kingdom 5 [:speedy :attack])
   (->Card :adventurer :kingdom 6 [])])


(def ^{:doc "Map of cards keyed by its names."
       :private true} card-map
  (into {} (map #(-> {(:name %) %}) cards)))


(def ^{:doc "Set of all card types."} types
  (set (map :type (vals card-map))))


(def ^{:doc "Set of all card names."} names
  (set (keys card-map)))


(def ^{:doc "Set of all card names that are not kingdoms."} all-but-kingdoms
  (set
    (keys
      (filter
        (fn [[_ v]] (not= :kingdom (:type v)))
        card-map))))


(defn- in-game-value [card]
  (cond
    (is? [:victory :victory-ish] card) 0
    (is? :treasure card) (:value card)
    :else (:cost card)))


(defn build-deck [cards]
  "Returns a list of unique cards given a map of {card name, number of copies}.
  Following metadata is generated for each card:
    + _id: unique identifier.
    + _inner_val: in-game for the player having this card in hand."
  (mapcat
    (fn [[k v]]
      (let [card (k card-map)]
        (repeatedly
          v
          #(assoc
            (apply ->Card (vals card))
            :_id (str (UUID/randomUUID))
            :_inner_val (in-game-value card)))))
    cards))