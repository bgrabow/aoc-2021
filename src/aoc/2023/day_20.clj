(ns aoc.2023.day-20
  (:require [aoc.util :as util]
            [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defmethod print-method PersistentQueue [x ^java.io.Writer writer]
  (print-method (vec x) writer))

(def input (util/read-input))

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
  (update state :pulses conj ["broadcaster" :low "button"]))

(defn broadcast
  [state node polarity]
  (-> state
    (update :pulses into (for [d (get-in state [:destinations node])]
                           [d polarity node]))))

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
             (update-in [:pulses] into (for [d (get-in state [:destinations node])]
                                         [d sent-polarity node]))))))

(defn conjunction
  [state node polarity origin])

(defn process-pulse
  [state]
  (if-let [[dest polarity origin] (peek (:pulses state))]
    (case (get (:nodes state) dest)
      "" (broadcast (update state :pulses pop) dest polarity)
      "%" (flip-flop (update state :pulses pop) dest polarity)
      "&" (conjunction (update state :pulses pop) dest polarity origin))
    state))

(->> (parse-input input)
  push-button
  (util/iterate-until-fixed process-pulse))

; TODO
; Initialize conjunction node state with :low for all source nodes
; or make a reverse map of each conjunction node's sources and lazily
; compute initial state for each source
