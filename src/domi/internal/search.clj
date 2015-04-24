(ns ^{:doc "This namespace contains generic functions to search within decks and
      query for specific card attributes."}
    domi.internal.search
  (:use (domi.internal [misc :only [vec-set]])))


(defn- filter-by-values [values cards]
  "Filter the cards with the given types, names and/or values."
  (let [inner-filter (fn [a c]
                       (let [v (a c)]
                         (if (coll? v)
                           (seq
                             (filter
                               #(some #{%} values) v))
                           (some #{v} values))))
        filter-by (fn [a]
                    (set
                      (filter
                        (partial inner-filter a) cards)))]
    (clojure.set/union
      (filter-by :type)
      (filter-by :name)
      (filter-by :value))))


(defn filter-from-deck [player val-s deck]
  "Filter the cards with the given type(s), name(s) or value(s) from a
  player's deck."
  (vec
    (filter-by-values
      (vec-set val-s) (deck @player))))

(defn filter-from-player [player val-s]
  (mapcat
    (partial filter-from-deck player val-s)
    [:discard :hand :player-deck]))

(defn is? [val-s card]
  "Return true if the cards has any of the give type, name or value."
  (seq
    (filter-by-values
      (vec-set val-s) (vector card))))


(defn all-in? [player items]
  "Return true if the player has any of the given items in any deck,
  false otherwise."
  (let [everything-a-player-has (flatten (vals @player))]
    (every?
      seq
      (map
        #(some #{%} everything-a-player-has)
        items))))


(defn lookup-deck [playable card]
  "Returns in which deck a card is, or nil if not found."
  (some
    (fn [deck]
      (when
        (some #{card} (val deck))
        (key deck)))
    @playable))


(defn- all-in-deck? [player deck cards]
  "Returns deck if the player has all the cards in deck,
  nil otherwise."
  (when
    (every?
      seq
      (map
        #(some #{%} (deck @player))
        cards))
    deck))


(defn resolve-deck [player cards]
  "If all the cards are in the same player's deck, returns that deck.
  Otherwise, return nil."
  (when-let [candidate-deck (lookup-deck player (first cards))]
    (all-in-deck? player candidate-deck cards)))