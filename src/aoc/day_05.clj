(ns aoc.day-05
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def example-input
  "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map #(map parse-long (re-seq #"\d+" %)))
       (map #(partition 2 %))))

(def input (util/read-input))

(comment
  (parse-input input))

(defn diagonal?
  [[[x1 y1] [x2 y2]]]
  (and (not= x1 x2)
       (not= y1 y2)))

(defn bidirectional-range
  "Create range from start (inclusive) to end (inclusive !!!). Start may be numerically greater than end,
  which results in a range that counts down from start to end."
  [start end]
  (if (> start end)
    (reverse (range end (inc start)))
    (range start (inc end))))

(defn line-segment
  [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (map vector (repeat x1) (bidirectional-range y1 y2))
    (= y1 y2) (map vector (bidirectional-range x1 x2) (repeat y1))
    :else (map vector (bidirectional-range x1 x2) (bidirectional-range y1 y2))))

(defn part-1
  []
  (->> (parse-input input)
       (remove diagonal?)
       (mapcat line-segment)
       (frequencies)
       (filter (comp (partial <= 2) second))
       (count)))

(defn part-2
  []
  (->> (parse-input input)
       (mapcat line-segment)
       (frequencies)
       (filter (comp (partial <= 2) second))
       (count)))
