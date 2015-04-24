(ns ^{:doc "This namespace knows the game execution flow."}
    domi.core
  (:use [domi.cards.effects :only [effect!]]
        [domi.decission.choose :only [choose-action choose-buy]]
        [domi.internal.misc :only [highest sum-by]]
        [domi.internal.search :only [filter-from-deck]]
        [domi.internal.turn :only [available? consume]]
        [domi.internal.state :only [draw! move! gain! move-all!]]))


(defn- play-actions! [player resources supply & oponents]
  "Performs the action phase: plays action cards until no more action cards
  or action resources are available.
  It causes side effects.
  Returns a list containing the player, new resources, supply and oponents."
  (if-let [action-card (and (available? resources :actions)
                            (choose-action player))]
    (do
      (move! player action-card :in-play)
      (apply play-actions! (effect!
                             action-card
                             player
                             (consume resources :actions)
                             supply
                             oponents)))
    [player resources supply oponents]))


(defn- play-buys!
  "Performs the buy phase: buy cards until no more money or buy resources
  are available.
  It causes side effects.
  Returns the updated player."
  ([player resources supply]
    (let [money-in-hand (+ (:coins resources)
                           (sum-by :value
                                   (filter-from-deck player :treasure :hand)))]
      (play-buys! player resources supply money-in-hand)))
  ([player resources supply money-in-hand]
    (let [spend (partial - money-in-hand)]
      (if-let [buy-card (and (available? resources :buys)
                             (choose-buy money-in-hand 2 supply))]
        (do
          (gain! supply buy-card player :discard)
          (play-buys! player
                      (consume resources :buys)
                      supply
                      (spend (:cost buy-card))))
        player))))


(defn- clean-up! [player]
  "Performs the clean up phase: discards all remaining cards from hand,
  and draws 5 new cards restoring the player deck if necessary."
  (->> player
       (move-all! :hand :discard)
       (move-all! :in-play :discard)
       (draw! 5)))


(defn- not-finished? [supply]
  "Checks if the end game conditions are met."
  (or
    (-> @supply :province count zero?)
    (->> @supply vals (map count) (filter zero?) count (= 3))))


(defn play [supply players default-resources]
  "Play Dominion!"
  (for [current-player (take-while
                         (not-finished? supply)
                         (cycle players))
        :let [oponents (remove current-player players)
              current-state [current-player
                             default-resources
                             supply
                             oponents]]]
    (->> current-state
         (apply play-actions!)
         (apply play-buys!)
         clean-up!)))
