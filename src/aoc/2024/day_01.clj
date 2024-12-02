(ns aoc.2024.day-01
  (:require [aoc.util :as util]))

(defn parse-lists
  []
  (->> (re-seq #"\d+" (util/read-input))
    (map parse-long)
    (partition 2)
    (util/transpose)
    (map sort)))

(defn part-1
  []
  (let [[left right] (parse-lists)]
    (reduce + (map (comp abs -) left right))))

(defn part-2
  []
  (let [[left right] (parse-lists)]
    (->> (select-keys (frequencies right) left)
      (map (fn score [[val freq]] (* val freq)))
      (reduce +))))
