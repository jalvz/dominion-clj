(ns ^{:doc "This namespace knows what the Kingdoms cards are for.
            Some domain knowledge is represented in effect functions,
            e.g.: behaviour for cards that have several use cases"}
    domi.cards.effects
  (:use [domi.decission.choose :only [choose-action choose-buy]]
        [domi.internal.misc :only [sum-by]]
        [domi.internal.search :only [filter-from-deck is?]]
        [domi.internal.turn :only [gain]]
        [domi.internal.state :only [draw!
                                    move!
                                    transfer!
                                    gain!
                                    move-all!
                                    reveal-and-move!
                                    trash-and-gain!]]))



(defn- counter? [player]
  "Checks that player does have a Moat in his/her hand that will counter
  an action-attack card."
  (seq
    (filter-from-deck
      player :moat :hand)))


(defmulti effect!
          "Perform the effect described by the card rule.
          Effects might mutate the player using the effect, the supply and/or
          the opponents."
          (fn [card _ _ _ _]
            (:name card)))


(defmethod
  effect! :cellar
  ^{:doc "+1 Action. Discard any number of cards. +1 Card per card discarded."}
  [_ player resources supply oponents]
  (let [cards-to-discard (filter-from-deck
                           player [:victory :victory-ish] :hand)
        count-fn #(count (:discard @player))
        before-count (count-fn)
        after-count-fn #(- (count-fn) before-count)]
    (move! player :discard cards-to-discard)
    [(draw! (after-count-fn) player)
     (gain resources {:actions 1})
     supply
     oponents]))


(defmethod
  effect! :chapel
  ^{:doc "Trash up to 4 cards from your hand."}
  [_ player resources supply oponents]
  (let [cards-to-trash
        (vec
          (take 4
                (filter-from-deck
                  player [:copper :curse :estate] :hand)))]
    (move! player :trash cards-to-trash)
    [player resources supply oponents]))


(defmethod
  effect! :moat
  ^{:doc "+2 Cards. When another player plays an Attack card, you may reveal
  this from your hand. If you do, you are unaffected by that Attack."}
  [_ player resources supply oponents]
  [(draw! 2 player) resources supply oponents])


(defmethod
  effect! :village
  ^{:doc "+1 Card, +2 Actions."}
  [_ player resources supply oponents]
  [(draw! 1 player)
   (gain resources {:actions 2})
   supply
   oponents])


(defmethod
  effect! :workshop
  ^{:doc "Gain a card costing up to 4."}
  [_ player resources supply oponents]
  (when-let [buy-card (choose-buy 4 4 supply)]
    (gain! supply player buy-card))
  [player resources supply oponents])


(defmethod
  effect! :chancellor
  ^{:doc "+2 Coins. You might inmediately put your deck into your discard
  pile."}
  [_ player resources supply oponents]
  (move-all! player :player-deck :discard)
  [player (gain resources {:coins 2}) supply oponents])


(defmethod
  effect! :woodcutter
  ^{:doc "+1 Buy, +2 Coins."}
  [_ player resources supply oponents]
  [player (gain resources {:buys 1 :coins 2}) supply oponents])


(defmethod
  effect! :smithy
  ^{:doc "+3 Cards."}
  [_ player resources supply oponents]
  [(draw! 3 player) resources supply oponents])


(defmethod
  effect! :bureucrat
  ^{:doc "Gain a Silver card, put it on top of your deck. Each other player
   reveals a Victory card and puts it on top of his deck (or reveals a hand
   with no Victory cards)."}
  [_ player resources supply oponents]
  (let [attack! (fn []
                  (map
                    (fn [oponent]
                      (let [victory-card
                            (first
                              (filter-from-deck
                                oponent :victory :hand))]
                        (if (and
                              victory-card
                              (not (counter? oponent)))
                          (move! oponent :player-deck victory-card)
                          oponent)
                        ))
                    oponents)
                  )]
    (gain! supply player :player-deck :silver)
    [player resources supply (attack!)]))


(defmethod
  effect! :feast
  ^{:doc "Trash this card. Gain a card costing up to 5 Coins."}
  [this-card player resources supply oponents]
  (when-let [gain-card-name (choose-buy 5 5 supply)]
    (trash-and-gain! supply player gain-card-name this-card))
  [player resources supply oponents])


(defmethod
  effect! :moneylender
  ^{:doc "Trash a Copper card from your hand. If you do, +3 Coins."}
  [_ player resources supply oponents]
  (if-let [copper-card (first (filter-from-deck player :copper :hand))]
    [(move! player :trash copper-card)
     (gain resources {:coins 3})
     supply
     oponents]
    [player resources supply oponents]))


(defmethod
  effect! :gardens
  ^{:doc " 1 VP for every 10 cards in your deck (rounded down)."}
  [_ player resources supply oponents]
  [player resources supply oponents]
  )


(defmethod
  effect! :throne-room
  ^{:doc "Choose an Action card in your hand. Play it twice."}
  [_ player resources supply oponents]
  (if-let [action (choose-action player)]
    (do
      (move! player :discard action)
      (apply effect!
             action
             (effect! action player resources supply oponents)))
    [player resources supply oponents]))


(defmethod
  effect! :militia
  ^{:doc "+2 Coins. Each other player discards down to 3 cards in his hand."}
  [_ player resources supply oponents]
  (let [alter-oponents! (fn []
                          (map
                            (fn [oponent]
                              (let [cards-over
                                    (- (count (:hand @oponent)) 3)
                                    cards-to-discard
                                    (take
                                      cards-over
                                      (sort-by :_inner_val (:hand @oponent)))]
                                (if (counter? oponent)
                                  oponent
                                  (move! oponent :discard cards-to-discard))))
                            oponents))]
    [player (gain resources {:coins 2}) supply (alter-oponents!)]))


(defmethod
  effect! :remodel
  ^{:doc "Trash a card from your hand. Gain a card costing up to 2 Coins more
  than the trashed card."}
  [_ player resources supply oponents]
  (let [to-trash (first (sort-by :cost (:hand @player)))
        up-to (+ (:cost to-trash) 2)]
    (when-let [gain-card-name (choose-buy up-to up-to supply)]
      (trash-and-gain! supply player gain-card-name to-trash))
    [player resources supply oponents]))



(defmethod
  effect! :spy
  ^{:doc "+1 Card, +1 Action. Each player (including you) reveals the top card
  of his deck and either discards it or puts it back, your choice"}
  [_ player resources supply oponents]
  (let [spy-deck! (fn [p fn]
                    (reveal-and-move! p fn :player-deck dec 1))
        retain-opponent? #(is? [:victory :victory-ish] %)
        retain-self? (complement retain-opponent?)
        alter-oponents! (fn []
                          (map
                            (fn [oponent]
                              (if (counter? oponent)
                                oponent
                                (spy-deck! oponent retain-opponent?)))
                            oponents))]
    [(spy-deck! player retain-self?)
     resources
     supply
     (alter-oponents!)]))


(defmethod
  effect! :thief
  ^{:doc "Each other player reveals the top 2 cards of his deck. If they
  revealed any Treasure cards, they trash one of them that you choose.
  You may gain any or all of these trashed cards. They discard the other
  revealed cards."}
  [_ player resources supply oponents]
  (let [steal! (fn []
                 (map
                   (fn [oponent]
                     (let [top-2-cards (-> (draw! 2 oponent :tmp) deref :tmp)
                           to-steal (first
                                      (filter
                                        #(is? [:silver :gold] %)
                                        top-2-cards))
                           to-discard (vec
                                        (remove #{to-steal} top-2-cards))]
                       (if (counter? oponent)
                         [oponent player]
                         (do
                           (move! oponent :discard to-discard)
                           (transfer! oponent player to-steal)))))
                   oponents))
        altered-players (steal!)]
    [(-> altered-players last second) resources supply (map first altered-players)]))


(defmethod
  effect! :council-room
  ^{:doc "+4 Cards, +1 Buy, Each other player draws a card."}
  [_ player resources supply oponents]
  [(draw! 4 player)
   (gain resources {:buys 1})
   supply
   (map #(draw! 1 %) oponents)])


(defmethod
  effect! :witch
  ^{:doc "+2 Cards, Each other player gains a Curse card."}
  [_ player resources supply oponents]
  (let [alter-oponents! (fn []
                          (map
                            (fn [opponent]
                              (if (counter? opponent)
                                opponent
                                (gain! supply opponent :discard :curse)))
                            oponents))]
    [(draw! 2 player) resources supply (alter-oponents!)]))


(defmethod
  effect! :mine
  ^{:doc "Trash a Treasure card from your hand. Gain a Treasure card costing
  up to 3 Coins more; put it into your hand."}
  [_ player resources supply oponents]
  (when-let [to-trash (first
                        (filter-from-deck player [:copper :silver] :hand))]
    (let [up-to (+ (:cost to-trash) 3)]
      (when-let [gain-card-name (choose-buy up-to up-to supply :treasure)]
        (trash-and-gain! supply player gain-card-name :hand to-trash))))
  [player resources supply oponents])


(defmethod
  effect! :festival
  ^{:doc "+2 Actions, +1 Buy, +2 Coins."}
  [_ player resources supply oponents]
  [player (gain resources {:actions 2 :buys 1 :coins 2}) supply oponents])


(defmethod
  effect! :laboratory
  ^{:doc "+2 Cards, +1 Action."}
  [_ player resources supply oponents]
  [(draw! 2 player) (gain resources {:actions 1}) supply oponents])


(defmethod
  effect! :library
  ^{:doc "+Draw until you have 7 cards in hand. You may set aside any Action
  cards drawn this way, as you draw them; discard the set aside cards after
  you finish drawing."}
  [_ player resources supply oponents]
  (let [to-reveal (- 7 (-> @player :hand count))
        retain? (fn [c] (or
                          (is? [:victory :treasure] c)
                          (pos? (:actions resources))))]
    [(reveal-and-move!
       player retain? :hand to-reveal)
     resources
     supply
     oponents]))


(defmethod
  effect! :market
  ^{:doc "+1 Card, +1 Action, +1 Buy, +1 Coin."}
  [_ player resources supply oponents]
  [(draw! 1 player)
   (gain resources {:actions 1 :buys 1 :coins 1})
   supply
   oponents])


(defmethod
  effect! :adventurer
  ^{:doc "Reveal cards from your deck until you reveal 2 Treasure cards.
  Put those Treasure cards in your hand and discard the other revealed cards."}
  [_ player resources supply oponents]
  [(reveal-and-move!
     player #(is? :treasure %) :hand 2)
   resources
   supply
   oponents])


