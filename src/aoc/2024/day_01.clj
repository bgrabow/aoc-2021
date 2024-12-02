(ns aoc.2024.day-01
  (:require [aoc.util :as util]))

(defn part-1
  []
  (let [[left right] (->> (re-seq #"\d+" (util/read-input))
                       (map parse-long)
                       (partition 2)
                       (util/transpose)
                       (map sort))]
    (reduce + (map (comp abs -) left right))))

(defn part-2
  []
  (let [[left right] (->> (re-seq #"\d+" (util/read-input))
                       (map parse-long)
                       (partition 2)
                       (util/transpose)
                       (map sort))]
    (->> (select-keys (frequencies right) left)
      (map (fn score [[val freq]] (* val freq)))
      (reduce +))))
