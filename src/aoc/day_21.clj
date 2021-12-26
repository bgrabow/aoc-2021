(ns aoc.day-21
  (:require [aoc.util :as util]))

(def input (util/read-input))

(defn to-space
  [x]
  (inc (rem (+ 10 (dec x)) 10)))

(defn next-deterministic-roll
  [x]
  (inc (rem x 100)))

(def next-player {:player-1 :player-2
                  :player-2 :player-1})

(defn update-player
  [player rolls]
  (let [position (to-space (+ (:position player) (reduce + rolls)))]
    {:position position
     :score (+ (:score player) position)}))

(comment
  (update-player (update-player {:score 0 :position 8} [1 2 3]) [7 8 9]))

(defn step
  [state]
  (let [[rolls [next-die]] (split-at 3 (iterate next-deterministic-roll (:die state)))]
    (-> state
        (update (:current-player state) update-player rolls)
        (update :current-player next-player)
        (assoc :die next-die))))

(defn lower-score
  [state]
  (min (get-in state [:player-1 :score])
       (get-in state [:player-2 :score])))

(defn part-1
  []
  (let [turns (->> (iterate step {:player-1       {:score 0 :position 8}
                                  :player-2       {:score 0 :position 9}
                                  :current-player :player-1
                                  :die            1})
                   (util/take-until
                     (fn [state]
                       (seq (filter #(<= 1000 (:score %)) ((juxt :player-1 :player-2) state))))))]
    (* (* 3 (dec (count turns)))
       (lower-score (last turns)))))

(comment
  (def wrong-answers
    {:too-low #{171500}}))
