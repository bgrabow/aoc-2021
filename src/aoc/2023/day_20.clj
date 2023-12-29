(ns aoc.2023.day-20
  (:require [aoc.util :as util]
            [clojure.string :as str]
            [clojure.data :as data])
  (:import (clojure.lang PersistentQueue)))

(defmethod print-method PersistentQueue [x ^java.io.Writer writer]
  (print-method (vec x) writer))

(def input (util/read-input))
(def example-1 "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a")
(def example-2 "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output\n")

(defn parse-line
  [acc s]
  (let [[_ type node destinations-string] (re-find #"(\W?)(\w+) -> (.+)" s)]
    (-> acc
      (update :nodes assoc node type)
      (update :destinations assoc node (str/split destinations-string #", ")))))

(defn parse-input
  [input]
  (->> (str/split-lines input)
    (reduce parse-line {:nodes {}
                        :destinations {}
                        :pulses (PersistentQueue/EMPTY)
                        :memory {}})))

(defn push-button
  [state]
  (-> state
    (update :pulses conj ["broadcaster" :low "button"])
    (update :button-presses (fnil inc 0))))

(defn broadcast
  [state node polarity]
  (-> state
    (update :pulses into (for [d (get-in state [:destinations node])]
                           [d polarity node]))))

(defn send-pulse
  [state polarity origin]
  (update state :pulses into
    (for [d (get-in state [:destinations origin])]
      [d polarity origin])))

(defn flip-flop
  [state node polarity]
  (case polarity
    :high state
    :low (let [old-state (get-in state [:memory node] :off)
               new-state (get {:off :on
                               :on :off}
                           old-state)
               sent-polarity (get {:off :low
                                   :on :high}
                               new-state)]
           (-> state
             (assoc-in [:memory node] new-state)
             (send-pulse sent-polarity node)))))

(defn conjunction
  [state node polarity origin]
  (let [new-state (-> state
                    (assoc-in [:memory node origin] polarity))
        new-pulse (if (every? #{:high} (vals (get-in new-state [:memory node])))
                    :low
                    :high)]
    (send-pulse new-state new-pulse node)))

(defn process-pulse
  [state]
  (if-let [[dest polarity origin] (peek (:pulses state))]
    (case (get (:nodes state) dest)
      "" (broadcast (update state :pulses pop) dest polarity)
      "%" (flip-flop (update state :pulses pop) dest polarity)
      "&" (conjunction (update state :pulses pop) dest polarity origin)
      nil (-> state
            (update :pulses pop)
            (update-in [:outputs polarity] (fnil conj #{}) [dest polarity origin])))
    state))

(defn init-memory
  [state]
  (update state :memory merge
    (apply merge-with merge
      (for [[origin dests] (:destinations state)
            dest dests
            :when (= "&" (get (:nodes state) dest))]
        {dest {origin :low}}))))

(defn count-pulses
  [history]
  (->> history
    (keep (comp second peek :pulses))
    frequencies))

(defn button-cycles
  [n state]
  (when (pos? n)
    (let [first-cycle (util/iterate-until-fixed process-pulse
                        (push-button state))]
      (lazy-cat
        first-cycle
        (button-cycles (dec n) (last first-cycle))))))

(comment
  (button-cycles 2 (init-memory (parse-input example-1))))

(defn part-1
  [input]
  (->> (parse-input input)
    init-memory
    (button-cycles 1000)
    count-pulses
    vals
    (apply *)))

(->> (parse-input input)
  init-memory
  (button-cycles 100)
  (drop-while #(empty? (filter (comp #{:low} second) (:outputs %))))
  (first))

(->> (parse-input input)
  init-memory
  (button-cycles 10000000)
  (drop-while #(empty? (get-in % [:outputs :low])))
  (first))

(let [init (init-memory (parse-input input))]
  (map (comp sort :memory)
    (filter #(empty? (:pulses %)) (button-cycles 10 init))))
