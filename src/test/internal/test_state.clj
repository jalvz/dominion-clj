(ns test.internal.test-state
  (:use [clojure.test]
        [test.test-helper]
        [domi.init]
        [domi.internal.search]
        [domi.internal.state]))


(deftest test-move
  (let [p (init-player "Alice")
        [c1 c2 c3] (take 3 (:hand @p))
        c4 (first (:player-deck @p))
        c5 (init-card :copper)]
    (move! p :discard c1)
    (move! p :discard [c2 c3])
    (move! p :trash [c1 c4])
    (move! p :trash c5)
    (move-all! p :player-deck :in-play)
    (are [deck card] (= deck (lookup-deck p card))
                     :discard c1
                     :discard c2
                     :discard c3
                     :in-play c4
                     nil c5)
    (are [rs cnt_fn] (= rs (cnt_fn p))
                     10 ocount
                     5 icount
                     2 hcount
                     3 dcount)))


(defn- empty-supply-mines [s]
  (dosync
    (alter s assoc-in [:mine] '())
    s))

(deftest test-gain
  (let [p (init-player "Alice")
        s ((comp empty-supply-mines init-supply) [:mine :feast :moat :chapel] 2)
        [c1 c2] (take 2 (:hand @p))
        c3 (first (:player-deck @p))
        c4 (init-card :copper)]
    (gain! s p :feast)                                      ; success
    (gain! s p :mine)                                       ; fail
    (gain! s p :hand :moat)                                 ; success
    (gain! s p :laboratory)                                 ; fail
    (trash-and-gain! s p :chapel c1)                        ; success
    (trash-and-gain! s p :chapel :hand c3)                  ; success
    (trash-and-gain! s p :chapel c4)                        ; fail
    (trash-and-gain! s p :laboratory c2)                    ; fail
    (trash-and-gain! s p :mine c2)                          ; fail
    (are [occurrences card-name deck]
      (= occurrences (count
                       (filter-from-deck p card-name deck)))
      1 :feast :discard
      0 :mine :discard
      1 :chapel :discard
      1 :chapel :hand
      1 :moat :hand
      0 :moat :discard)
    (are [rs cnt_fn] (= rs (cnt_fn p))
                     14 ocount
                     6 hcount
                     2 dcount
                     2 tcount)))


(deftest test-draw
  (let [p (init-player "Alice")
        tst-fn #(and
                 (= %1 (hcount p))
                 (= %2 (kcount p))
                 (= (- 10 (+ %1 %2)) (dcount p))
                 (= 10 (ocount p)))]
    (draw! 3 p)
    (are [in-hand in-deck]
      (tst-fn in-hand in-deck)
      8 2)

    (draw! 3 p)
    (are [in-hand in-deck]
      (tst-fn in-hand in-deck)
      10 0)

    (move-all! p :hand :discard)
    (draw! 4 p)
    (are [in-hand in-deck]
      (tst-fn in-hand in-deck)
      4 6)

    (move-all! p :hand :discard)
    (draw! 4 p)
    (are [in-hand in-deck]
      (tst-fn in-hand in-deck)
      4 2)

    (move-all! p :hand :discard)
    (draw! 4 p)
    (are [in-hand in-deck]
      (tst-fn in-hand in-deck)
      4 6)))









