(ns ^{:doc "This namespace knows how to initialise the components to start a
            Dominion game."}
domi.init
  (:require (domi.cards [card :as cards])))


(defrecord Player [name player-deck hand in-play discard trash])

(defn init-player [name]
  "Returns a new player ready to play."
  (let [initial-deck (->> {:copper 7 :estate 3}
                          cards/build-deck
                          shuffle)
        hand (take 5 initial-deck)
        player-deck (drop 5 initial-deck)]
    (ref
      (->Player
        name player-deck hand '() '() '()))))

(defmethod clojure.core/print-method Player [p writer]
  "Pretty print player"
  (.write writer (str [(str (:name p) " (" (-> p :trash count) ")")
                       (->> p
                            ((juxt :player-deck :discard :hand))
                            flatten
                            (map :name)
                            frequencies
                            (into (sorted-map)))
                       (:hand p)])))
;(into
;  {}
;  (for [[k v] p
;        :when (and
;                (not= :name k)
;                (seq v))]
;    {k (count v)}))])))



;  (.write writer (str {(:name p)
;                  (reduce
;                    #(into %1 {%2
;                               (->> p
;                                    %2
;                                    (map :name)
;                                    frequencies)})
;                    {}
;                    (for [[k v] p
;                          :when (and
;                                  (not= :name k)
;                                  (seq v))]
;                      k))})))
;


(defrecord TurnResources [actions buys coins])

(defn init-resources []
  "Returns the resources for a player at the start of his/her turn."
  (->TurnResources 1 1 0))



(defrecord Supply [])

(defmethod clojure.core/print-method Supply [s writer]
  "Pretty print supply"
  (.write writer (str "supply "
                      (count (:province s)))))



(defn- copies-by-card [card-name n]
  "Returns the number of copies for the given card-name when n players are in
  the game. This configures the initial setup."
  (get {:copper   60
        :silver   40
        :gold     30
        :estate   (min 12 (* 4 n))
        :duchy    (min 12 (* 4 n))
        :province (min 12 (* 4 n))
        :curse    (* 10 (- n 1))}
       card-name
       10))

(defn- build-supply-as-map [card-names number-of-players]
  "Returns a map of {card name, [cards]} that represents a supply
  (collection of cards that players can gain) for the game."
  (reduce
    #(assoc
      %1
      %2
      (cards/build-deck
        {%2 (copies-by-card %2 number-of-players)}))
    {} card-names))


(defn init-supply [kingdom-cards number-of-players]
  "Returns a supply for number-of-players with the given kingdom-cards."
  (let [all-cards (concat kingdom-cards cards/all-but-kingdoms)]
    (-> all-cards
        (build-supply-as-map number-of-players)
        map->Supply
        ref)))


(def simple [:cellar :market :militia :mine :moat :remodel :smithy :village
             :woodcutter])

(def big-money [:adventurer :bureucrat :chancellor :chapel :feast :laboratory
                :market :mine :moneylender :throne-room])

(def interaction [:bureucrat :chancellor :council-room :festival :library
                  :militia :moat :spy :thief :village])

(def size-distortion [:cellar :chapel :feast :gardens :laboratory :thief
                      :village :witch :woodcutter :workshop])

(def village-square [:bureucrat :cellar :festival :library :market :remodel
                     :smithy :throne-room :village :woodcutter])
