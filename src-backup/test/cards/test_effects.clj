(ns test.test-effects
  (:use [clojure.test]
        [test.test-helper]
        [domi.init]
        [domi.internal.search]
        [domi.internal.turn]
        [domi.cards.card]
        [domi.cards.effects]))


(defn- init [card-name]
  (let [p1 (init-player "Player")
        p2 (init-player "Oponent 1")
        s (init-supply
            (clojure.set/difference names all-but-kingdoms)
            2)
        r (init-resources)
        c (init-card card-name)]
    (addc! p1 c :in-play)
    [c p1 r s [p2]]))

(defn- init-n-cards [card-name-1 & other-card-names]
  (let [[c p r s o] (init card-name-1)]
    (add-all! p :hand other-card-names)
    [c p r s o]))

(defn- init-two-oponents [card-name]
  (let [p1 (init-player "Player")
        p2 (init-player "Oponent 1")
        p3 (init-player "Oponent 2")
        s (init-supply
            (clojure.set/difference names all-but-kingdoms)
            2)
        r (init-resources)
        c (init-card card-name)]
    (addc! p1 c :in-play)
    [c p1 r s [p2 p3]]))

(defn- consume-action [[c p r s o]]
  [c p (consume r :actions) s o])

(defn- full-hand [[c p r s o]]
  (put1-all! p)
  [c p r s o])

(defn- put-province-in-player-hand [[c p r s o]]
  (put1! p [:province] :hand)
  [c p r s o])

(defn- put-province-in-player-deck [[c p r s o]]
  (put1! p [:province] :player-deck)
  [c p r s o])

(defn- put-gold-and-victories-in-player-deck [[c p r s o]]
  (put1! p [:gold :province :duchy :estate] :player-deck)
  [c p r s o])

(defn- put-gold-in-player-discard [[c p r s o]]
  (put1! p [:gold] :discard)
  [c p r s o])

(defn- put-gold-in-player-deck [[c p r s o]]
  (put1! p [:gold] :player-deck)
  [c p r s o])

(defn- add-smithy-to-player-deck [[c p r s o]]
  (add! p :smithy :player-deck)
  [c p r s o])

(defn- full-deck [[c p r s o]]
  (put1-all! p :player-deck)
  [c p r s o])

(defn- put-province-in-opponents-hand [[c p r s o]]
  (let [ifn (fn [] (map #(put1! % [:province] :hand) o))]
    [c p r s (ifn)]))

(defn- put-province-in-opponents-deck [[c p r s o]]
  (let [ifn (fn [] (map #(put1! % [:province] :player-deck) o))]
    [c p r s (ifn)]))

(defn- add-moat-to-opponents-hand [[c p r s o]]
  (let [ifn (fn [] (map #(add! % :moat :hand) o))]
    [c p r s (ifn)]))

(defn- put-gold-in-opponents-hand [[c p r s o]]
  (let [ifn (fn [] (map #(put1! % [:gold] :hand) o))]
    [c p r s (ifn)]))

(defn- put-gold-in-opponents-deck [[c p r s o]]
  (let [ifn (fn [] (map #(put1! % [:gold] :player-deck) o))]
    [c p r s (ifn)]))

(defn- add-gold-to-opponents-deck [[c p r s o]]
  (let [ifn (fn [] (map #(add! % :gold :player-deck) o))]
    [c p r s (ifn)]))

(defn- drop-curses-from-supply [[c p r s o]]
  (put1! s [] :curse)
  [c p r s o])


(defn- kick [[c p r s o]]
  (effect! c p r s o))


(deftest cellar
  (let [[p r _ _] ((comp kick full-deck full-hand init) :cellar)]
    (is (= 5 (dcount p)))
    (is (= 27 (kcount p)))
    (is (= 32 (hcount p)))
    (is (= 2 (:actions r)))))

(deftest chapel
  (let [[p _ _ _] ((comp kick full-hand init) :chapel)]
    (is (= 3 (tcount p)))
    (is (= 29 (hcount p)))))

(deftest moat
  (let [[p _ _ _] ((comp kick init) :moat)]
    (is (= 7 (hcount p)))
    (is (= 3 (kcount p)))))

(deftest village
  (let [[p r _ _] ((comp kick init) :village)]
    (is (= 6 (hcount p)))
    (is (= 4 (kcount p)))
    (is (= 3 (:actions r)))))

(deftest workshop
  (let [[p _ s _] ((comp kick init) :workshop)]
    (is (= 1 (dcount p)))
    (is (= 4 (-> @p :discard first :cost)))
    (is (= 413 (->> @s (map val) flatten count)))))

(deftest woodcutter
  (let [[_ r _ _] ((comp kick init) :woodcutter)]
    (is (= 2 (:buys r)))
    (is (= 2 (:coins r)))))

(deftest chancellor
  (let [[p r _ _] ((comp kick init) :chancellor)]
    (is (= 2 (:coins r)))
    (is (= 0 (kcount p)))
    (is (= 5 (dcount p)))))

(deftest smithy
  (let [[p _ _ _] ((comp kick init) :smithy)]
    (is (= 8 (hcount p)))
    (is (= 2 (kcount p)))
    )
  )

(deftest bureucrat-attack-sucess
  (let [[p _ s o] ((comp kick
                         put-province-in-opponents-hand
                         init-two-oponents) :bureucrat)]
    (is (= 39 (count (:silver @s))))
    (is (= 6 (kcount p)))
    (is (= :silver (-> @p :player-deck first :name)))
    (map
      #(is (= :province
              (-> @% :player-deck first :type))) o)
    (map #(is (= 6 (kcount %))) o)
    (map #(is (= 0 (hcount %))) o)))

(deftest bureucrat-attack-fails-1
  (let [[p _ s o] ((comp kick
                         add-moat-to-opponents-hand
                         put-province-in-opponents-hand
                         init-two-oponents) :bureucrat)]
    (is (= 39 (count (:silver @s))))
    (is (= 6 (kcount p)))
    (is (= :silver (-> @p :player-deck first :name)))
    (map #(is (= 5 (kcount %))) o)
    (map #(is (= 2 (hcount %))) o)))

(deftest bureucrat-attack-fails-2
  (let [[p _ s o] ((comp kick
                         put-gold-in-opponents-hand
                         init-two-oponents) :bureucrat)]
    (is (= 39 (count (:silver @s))))
    (is (= 6 (kcount p)))
    (is (= :silver (-> @p :player-deck first :name)))
    (map #(is (= 5 (kcount %))) o)
    (map #(is (= 1 (hcount %))) o)))

(deftest feast
  (let [[p _ s _] ((comp kick init) :feast)]
    (is (= 1 (tcount p)))
    (is (= :feast (-> @p :trash first :name)))
    (is (= 1 (dcount p)))
    (is (= 5 (-> @p :discard first :cost)))
    (let [gained (-> @p :discard first :name)
          expected (if (= gained :duchy) 7 9)]
      (is (= expected (count (gained @s)))))))

(deftest moneylender-success
  (let [[p r _ _] ((comp kick init) :moneylender)]
    (is (= 4 (hcount p)))
    (is (= 1 (tcount p)))
    (is (= :copper (-> @p :trash first :name)))
    (is (= 3 (:coins r)))))

(deftest moneylender-fails
  (let [[p r _ _] ((comp kick
                         put-province-in-player-hand
                         init) :moneylender)]
    (is (= 1 (hcount p)))
    (is (= 0 (tcount p)))
    (is (= 0 (:coins r)))))

(deftest throne-room-market
  (let [[p r _ _] ((comp kick init-n-cards)
                    :throne-room :market)]
    (is (= 1 (dcount p)))
    (is (= :market (-> @p :discard first :name)))
    (is (= 7 (hcount p)))
    (is (= 3 (kcount p)))
    (is (= 3 (:actions r)))
    (is (= 3 (:buys r)))
    (is (= 2 (:coins r)))))

(deftest throne-room-feast
  (let [[p _ _ _] ((comp kick init-n-cards)
                    :throne-room :feast)]
    (is (= 5 (hcount p)))
    (is (= 1 (tcount p)))
    (is (= :feast (-> @p :trash first :name)))
    (is (= 2 (dcount p)))
    (is (every? #(= % 5) (map :cost (:discard @p))))))

(deftest throne-room-fails
  (let [[p _ _ _] ((comp kick init) :throne-room)]
    (is (= 5 (hcount p)))
    (is (= 5 (kcount p)))
    (is (= 0 (dcount p)))
    (is (= 0 (tcount p)))))

(deftest militia-success
  (let [[_ r _ o] ((comp kick init-two-oponents) :militia)]
    (map #(is (= 3 (hcount %))) o)
    (map #(is (= 2 (dcount %))) o)
    (is (= 2 (:coins r)))))

(deftest militia-fails-1
  (let [[_ r _ o] ((comp kick
                         add-moat-to-opponents-hand
                         init-two-oponents) :militia)]
    (map #(is (= 5 (hcount %))) o)
    (map #(is (= 0 (dcount %))) o)
    (is (= 2 (:coins r)))))

(deftest militia-fails-2
  (let [[_ r _ o] ((comp kick
                         put-province-in-opponents-hand
                         init-two-oponents) :militia)]
    (map #(is (= 1 (hcount %))) o)
    (map #(is (= 0 (dcount %))) o)
    (is (= 2 (:coins r)))))

(deftest remodel-success
  (let [[p _ _ _] ((comp kick init) :remodel)]
    (is (= 4 (hcount p)))
    (is (= 1 (tcount p)))
    (is (= :copper (-> @p :trash first :name)))
    (is (= 1 (dcount p)))
    (is (= 2 (-> @p :discard first :cost)))))

(deftest remodel-fails
  (let [[p _ _ _] ((comp kick
                         put-province-in-player-hand
                         init) :remodel)]
    (is (= 1 (hcount p)))
    (is (= :province (-> @p :hand first :name)))))

(deftest spy-victories
  (let [[p _ _ o] ((comp kick
                         put-province-in-opponents-deck
                         put-province-in-player-deck
                         init-two-oponents) :spy)]
    (is (= 0 (kcount p)))
    (is (= 1 (dcount p)))
    (map #(is (= 0 (dcount %))) o)
    (map #(is (= 1 (kcount %))) o)))

(deftest spy-golds
  (let [[p _ _ o] ((comp kick
                         put-gold-in-opponents-deck
                         put-gold-in-player-deck
                         init-two-oponents) :spy)]
    (is (= 1 (kcount p)))
    (is (= 0 (dcount p)))
    (map #(is (= 1 (dcount %))) o)
    (map #(is (= 0 (kcount %))) o)))

(deftest spy-golds-counter
  (let [[p _ _ o] ((comp kick
                         add-moat-to-opponents-hand
                         put-gold-in-opponents-deck
                         put-gold-in-player-deck
                         init-two-oponents) :spy)]
    (is (= 1 (kcount p)))
    (is (= 0 (dcount p)))
    (map #(is (= 0 (dcount %))) o)
    (map #(is (= 1 (kcount %))) o)))

(deftest thief-lucky
  (let [[p _ _ o] ((comp kick
                         add-gold-to-opponents-deck
                         put-gold-in-opponents-deck
                         init-two-oponents) :thief)]
    (is (= 2 (count (filter-from-deck p :gold :discard))))
    (map #(is (= 1 (dcount %))) o)
    (map #(is (= 0 (kcount %))) o)))

(deftest thief-unlucky
  (let [[p _ _ o] ((comp kick
                         init-two-oponents) :thief)]
    (is (= 0 (dcount p)))
    (map #(is (= 2 (dcount %))) o)
    (map #(is (= 3 (kcount %))) o)))

(deftest thief-incomplete
  (let [[p _ _ o] ((comp kick
                         put-gold-in-opponents-deck
                         init-two-oponents) :thief)]
    (is (= 2 (count (filter-from-deck p :gold :discard))))
    (map #(is (= 0 (dcount %))) o)
    (map #(is (= 0 (kcount %))) o)))

(deftest thief-counter
  (let [[p _ _ o] ((comp kick
                         add-moat-to-opponents-hand
                         put-gold-in-opponents-deck
                         init-two-oponents) :thief)]
    (is (= 0 (dcount p)))
    (map #(is (= 0 (dcount %))) o)
    (map #(is (= 5 (kcount %))) o)))

(deftest laboratory
  (let [[p r _ _] ((comp kick init) :laboratory)]
    (is (= 7 (hcount p)))
    (is (= 3 (kcount p)))
    (is (= 2 (:actions r)))))

(deftest council-room
  (let [[p r _ o] ((comp kick init-two-oponents) :council-room)]
    (is (= 9 (hcount p)))
    (is (= 1 (kcount p)))
    (is (= 2 (:buys r)))
    (map #(is (= 6 (hcount %))) o)
    (map #(is (= 4 (kcount %))) o)))

(deftest witch-attack-success
  (let [[p _ _ o] ((comp kick init-two-oponents) :witch)]
    (is (= 7 (hcount p)))
    (is (= 3 (kcount p)))
    (map #(is (= 1 (dcount %))) o)
    (map
      #(is (= :curse
              (-> @% :discard first :name))) o)))

(deftest witch-attack-fail-1
  (let [[p _ _ o] ((comp kick
                         add-moat-to-opponents-hand
                         init-two-oponents) :witch)]
    (is (= 7 (hcount p)))
    (is (= 3 (kcount p)))
    (map #(is (= 0 (dcount %))) o)))

(deftest witch-attack-fail-2
  (let [[p _ _ o] ((comp kick
                         drop-curses-from-supply
                         init-two-oponents) :witch)]
    (is (= 7 (hcount p)))
    (is (= 3 (kcount p)))
    (map #(is (= 0 (dcount %))) o)))

(deftest mine-success
  (let [[p _ _ _] ((comp kick init) :mine)]
    (is (= 5 (hcount p)))
    (is (= 1 (tcount p)))
    (is (seq (filter-from-deck p :silver :hand)))
    (is (= :copper (-> @p :trash first :name)))))

(deftest mine-fails
  (let [[p _ _ _] ((comp kick
                         put-province-in-player-hand
                         init) :mine)]
    (is (= 1 (hcount p)))
    (is (= 0 (tcount p)))))

(deftest festival
  (let [[_ r _ _] ((comp kick init) :festival)]
    (is (= 3 (:actions r)))
    (is (= 2 (:buys r)))
    (is (= 2 (:coins r)))))

(deftest market
  (let [[p r _ _] ((comp kick init) :market)]
    (is (= 6 (hcount p)))
    (is (= 4 (kcount p)))
    (is (= 2 (:actions r)))
    (is (= 2 (:buys r)))
    (is (= 1 (:coins r)))))

(deftest library
  (let [[p _ _ _] ((comp kick
                         put-gold-and-victories-in-player-deck
                         init)
                    :library)]
    (is (= 7 (hcount p)))
    (is (= 2 (kcount p)))))

(deftest library-shuffle-in-the-middle
  (let [[p _ _ _] ((comp kick
                         put-gold-in-player-discard
                         put-gold-in-player-deck
                         init)
                    :library)]
    (is (= 7 (hcount p)))
    (is (= 2 (count (filter-from-deck p :gold :hand))))
    (is (= 0 (dcount p)))
    (is (= 0 (kcount p)))))

(deftest library-incomplete
  (let [[p _ _ _] ((comp kick
                         put-province-in-player-hand
                         put-gold-and-victories-in-player-deck
                         init)
                    :library)]
    (is (= 5 (hcount p)))
    (is (= 0 (kcount p)))))

(deftest library-hold-on-action
  (let [[p _ _ _] ((comp kick
                         add-smithy-to-player-deck
                         put-gold-in-player-deck
                         init)
                    :library)]
    (is (= 7 (hcount p)))
    (is (= 0 (kcount p)))))

(deftest library-ignore-action
  (let [[p _ _ _] ((comp kick
                         add-smithy-to-player-deck
                         put-gold-in-player-deck
                         consume-action
                         init)
                    :library)]
    (is (= 6 (hcount p)))
    (is (= 1 (dcount p)))
    (is (= 0 (kcount p)))))

(deftest adventurer
  (let [[p _ _ _] ((comp kick
                         full-deck
                         put-province-in-player-hand
                         init)
                    :adventurer)]
    (is (= 3 (hcount p)))
    (is (= 2 (count (filter-from-deck p :treasure :hand))))
    (is (= 30 (+ (dcount p)
                 (kcount p))))))

(deftest adventurer-shuffle-in-the-middle
  (let [[p _ _ _] ((comp kick
                         put-gold-and-victories-in-player-deck
                         put-gold-in-player-discard
                         put-province-in-player-hand
                         init)
                    :adventurer)]
    (is (= 3 (hcount p)))
    (is (= 2 (count (filter-from-deck p :gold :hand))))
    (is (= 3 (+ (dcount p)
                (kcount p))))))

(deftest adventurer-incomplete
  (let [[p _ _ _] ((comp kick
                         put-gold-and-victories-in-player-deck
                         put-province-in-player-hand
                         init)
                    :adventurer)]
    (is (= 2 (hcount p)))
    (is (= 1 (count (filter-from-deck p :gold :hand))))
    (is (= 3 (+ (dcount p)
                (kcount p))))))

;(run-tests)