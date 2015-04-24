(ns ^{:doc "This namespace contains some objects and stuff to test easily with
      a REPL console."}
    test.repl-ay
  (:use [domi.cards.card]
        [domi.init]))



(def p1 (init-player "Alice"))


(def p2 (init-player "Bob"))


(def p3 (init-player "Antonio"))


(def fs2 (init-supply
         (clojure.set/difference names all-but-kingdoms)
         2))


(def fs3 (init-supply
           (clojure.set/difference names all-but-kingdoms)
           3))


(def r (init-resources))



(defn p [x]
  (clojure.pprint/pprint x))
