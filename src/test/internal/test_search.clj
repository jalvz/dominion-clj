(ns test.internal.test-search
  (:use [clojure.test]
        [test.test-helper]
        [domi.init]
        [domi.internal.search]))


(deftest test-filter
  (let [p (init-player "Alice")]
    (put1-all! p)
    (are [cnt coll] #(= (cnt (count coll)))
                    25 (filter-from-deck p :kingdom :hand)
                    1 (filter-from-deck p :province :hand)
                    2 (filter-from-deck p [:silver :gold] :hand)
                    2 (filter-from-deck p [:victory-ish :smithy :gardens] :hand))
    (are [k v coll] (every? #(= v (k %)) coll)
                    :type :victory (filter-from-deck p :victory :hand)
                    :name :curse (filter-from-deck p :curse :hand)
                    :name :gardens (filter-from-deck p :victory-ish :hand)
                    :name :copper (filter-from-deck p :treasure :player-deck)
                    :name :estate (filter-from-deck p :victory :player-deck)
                    :type :treasure (filter-from-deck p [:copper :silver :gold] :hand))
    (are [rs coll] (empty?
                     (clojure.set/difference
                       rs (set (map #(:name %) coll))))
                   #{:estate}
                   (filter-from-deck p [:victory :kingdom] :player-deck)
                   #{:smithy :copper :silver :gold}
                   (filter-from-deck p [:smithy :treasure] :hand)
                   #{:curse :estate :duchy :province :gardens}
                   (filter-from-deck p [:victory-ish :victory] :hand)
                   #{:laboratory :copper :duchy}
                   (filter-from-deck p [:laboratory :copper :duchy] :hand))))

(deftest test-is?
  (are [attr cname] (is? attr (init-card cname))
                    :victory :curse
                    :victory-ish :gardens
                    :treasure :copper
                    :kingdom :smithy
                    :smithy :smithy))

(deftest test-lookup
  (let [p (init-player "Alice")]
    (put1! p [:copper :moat :mine :curse] :discard)
    (let [in-discard (:discard @p)
          in-hand (:hand @p)
          not-in-player [(init-card :mine) (init-card :gold)]]
          (are [rs coll] (= rs (all-in? p coll))
               true in-hand
               true (take 2 in-discard)
               true (map last [in-hand in-discard])
               false not-in-player
               false (first not-in-player)
               false (map first [in-discard not-in-player]))
          (are [rs card] (= rs (lookup-deck p card))
                         :discard (first in-discard)
                         :hand (first in-hand)
                         nil (first not-in-player))
          (are [rs coll] (= rs (resolve-deck p coll))
                         :discard in-discard
                         :hand (drop 1 in-hand)
                         nil not-in-player
                         nil (map first [in-hand in-discard])))))



