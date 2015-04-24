(ns ^{:doc "This namespace contains functions with alter the state of the
      entities in the game (supply, players)."}
    domi.internal.state
  (:use (domi.internal [misc :only [vec-set]]
          [search :only [all-in? resolve-deck lookup-deck]]
          )))



(defn- internal-move! [player-1 to player-2 cards]
  "Moves cards to 'to' player's deck.
  All cards must be in the same player's deck."
  (if-let [from (resolve-deck player-1 cards)]
    (dosync
      (alter player-1 update-in [from] #(remove cards %))
      (alter player-2 update-in [to] into cards))
    (prn (str "State not changed - card(s) not in any deck " cards))
    ))


(defn move! [player to card-s]
  (internal-move! player to player (vec-set card-s))
  player)


(defn transfer! [oponent player card-s]
  (internal-move! oponent :discard player (vec-set card-s))
  [oponent player])


(defn move-all! [player from to]
  "Moves all player's cards from one deck to another."
  (dosync
    (alter player update-in [to] concat (from @player))
    (alter player assoc-in [from] '())
    player)
  )


(defn gain!
  "Gains a card from the supply if available and puts it in player's to deck."
  ([supply player card-name]
    (gain! supply player :discard card-name))
  ([supply player to card-name]
    (if-let [card (first (card-name @supply))]
      (dosync
        (alter supply update-in [card-name] #(remove #{card} %))
        (alter player update-in [to] conj card))
      (prn
        (str "State not changed - can't gain card " card-name)))
    player))


(defn trash-and-gain!
  "Gains a card from the supply and trash the given card.
  Gained card goes to discard pile, unless otherwise specified.
  If is not possible to get the card from the supply, it will throw an error."
  ([supply player gain-card-name trash-card]
    (trash-and-gain! supply player gain-card-name :discard trash-card))
  ([supply player gain-card-name to-deck trash-card]
    (let [gain-card (first (gain-card-name @supply))
          trash-card-deck (lookup-deck player trash-card)]
      ;(if (and gain-card trash-card-deck (not= :trash trash-card-deck))
      (if (and gain-card trash-card-deck)
        (dosync
          (alter supply update-in [gain-card-name] #(remove #{gain-card} %))
          (alter player update-in [to-deck] conj gain-card)
          (alter player update-in [trash-card-deck] #(remove #{trash-card} %))
          (alter player update-in [:trash] conj trash-card))
        (prn
          (str "State not changed - can't gain card " gain-card-name)))
      player)))


(defn reveal-and-move!
  "Reveals the top card from the player's deck and moves it to the
  'success-destination' if condition-fn' is hold or discard'
  otherwise."
  ([player condition-fn success-destination n]
    (reveal-and-move!
      player condition-fn success-destination identity n))
  ([player condition-fn success-destination fail-continue-fn n]
    (let [exec (fn self [player times can-shuffle]
                 (let [not-done? (pos? times)
                       top-card (first (:player-deck @player))]
                   (cond
                     (and not-done? top-card)
                     ; resolve card and recur
                     (if (condition-fn top-card)
                       (self
                         (move! player success-destination top-card)
                         (dec times)
                         can-shuffle)
                       (self
                         (move! player :discard top-card)
                         (fail-continue-fn times)
                         can-shuffle))
                     (and not-done? (not top-card) can-shuffle)
                     ; shuffle in the middle
                     (self
                       (dosync
                         (alter player
                           update-in [:player-deck]
                           concat (shuffle (:discard @player)))
                         (alter player
                           assoc-in [:discard]
                           '())
                         player)
                       times
                       false)
                     :else player)))]
      (exec player n true))))


(defn draw!
  "Draw n cards to player. If the player deck is empty, then the discard pile
  is shuffled and used as new player deck."
  ([n player] (draw! n player :hand))
  ([n player to]
    (dosync
      ;up to n from deck to destination
      (alter player
        update-in [to]
        concat (take n (:player-deck @player)))
      (alter player
        update-in [:player-deck]
        (partial drop n))
      (when (empty? (:player-deck @player))
        (let [missing (- n (count (to @player)))]
          ;all from discard to deck
          (alter player
            update-in [:player-deck]
            concat (shuffle (:discard @player)))
          (alter player
            assoc-in [:discard]
            '())
          ;remaining from deck to destination
          (alter player
            update-in [to]
            concat (take missing (:player-deck @player)))
          (alter player
            update-in [:player-deck]
            (partial drop missing))))
      player)))
