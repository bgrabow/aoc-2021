(ns aoc.2023.day-04
  (:require [aoc.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (util/read-input))

(defn part-1
  [input]
  (->> (str/split-lines input)
    (map #(let [[_ winning have] (re-find #".+:(.+)\|(.+)" %)
                winning (set (map parse-long (re-seq #"\d+" winning)))
                have (set (map parse-long (re-seq #"\d+" have)))
                matches (count (set/intersection winning have))]
            (if (zero? matches)
              0
              (long (Math/pow 2 (dec matches))))))
    (reduce +)))

(defn part-2
  [input]
  (->> (str/split-lines input)
    (map #(let [[_ card winning have] (re-find #"(\d+):(.+)\|(.+)" %)
                winning (set (map parse-long (re-seq #"\d+" winning)))
                have (set (map parse-long (re-seq #"\d+" have)))
                matches (count (set/intersection winning have))]
            [(parse-long card) matches]))
    (reduce
      (fn [acc [card matches]]
        (let [cards-to-add (range (inc card) (inc (+ matches card)))]
          (merge-with + (zipmap cards-to-add (repeat (get acc card))) acc)))
      (zipmap
        (range 1 (inc (count (str/split-lines input))))
        (repeat 1)))
    (vals)
    (reduce +)))
