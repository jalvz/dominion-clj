(ns ^{:doc "This namespace knows the game execution flow."}
domi.core
  (:use [domi.cards.effects :only [effect!]]
        [domi.decission.choose :only [choose-action choose-buy]]
        [domi.internal.misc :only [highest sum-by]]
        [domi.cards.card :only [cost]]
        [domi.internal.search :only [filter-from-deck filter-from-player]]
        [domi.internal.turn :only [available? consume]]
        [domi.internal.state :only [draw! move! gain! move-all!]]))


(defn- play-actions! [player resources supply oponents]
  "Performs the action phase: plays action cards until no more action cards
  or action resources are available.
  It causes side effects.
  Returns a list containing the player, new resources, supply and oponents."
  (if-let [action-card (and (available? resources :actions)
                            (choose-action player))]
    (do
      (move! player :in-play action-card)
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
  ([player resources supply _]
   (let [money-in-hand (+ (:coins resources)
                          (sum-by :value
                                  (filter-from-deck player :treasure :hand)))]
     (play-buys! player resources supply _ money-in-hand)))
  ([player resources supply _ money-in-hand]
   (let [spend (partial - money-in-hand)]
     (if-let [buy-card (and (available? resources :buys)
                            (choose-buy money-in-hand 2 supply))]
       (do
         (gain! supply player buy-card)
         (play-buys! player
                     (consume resources :buys)
                     supply
                     _
                     (spend (cost buy-card))))
       player))))


(defn- clean-up! [player]
  "Performs the clean up phase: discards all remaining cards from hand,
  and draws 5 new cards restoring the player deck if necessary."
  (-> player
      (move-all! :hand :discard)
      (move-all! :in-play :discard)
      ((partial draw! 5))))


(defn- not-finished? [supply]
  "Checks if the end game conditions are met."
  (not
    (or
      (-> @supply :province count zero?)
      (->> @supply vals (map count) (filter zero?) count (= 3)))))


(defn play [supply players default-resources]
  "Play Dominion!"
  (for [current-player (cycle players)
        :while (not-finished? supply)
        :let [oponents (remove #{current-player} players)
              current-state [current-player
                             default-resources
                             supply
                             oponents]]]
    (->> current-state
         (apply play-actions!)
         (apply play-buys!)
         clean-up!)))


(defn score [players]
  (let [score-victories (fn [p] (sum-by
                                  :value
                                  (filter-from-player p :victory)))
        score-gardens (fn [p]
                        (*
                          (count
                            (filter-from-player p :gardens))
                          (clojure.lang.Numbers/quotient
                            (sum-by
                              #(-> @p % count)
                              [:discard :hand :player-deck])
                            10)))
        score-all (fn [p] (+ (score-gardens p)
                             (score-victories p)))]
    (map score-all players)))
