(ns aoc.day-01
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(defn count-increasing
  [xs]
  (->> xs
       (partition 2 1)
       (map #(apply < %))
       (filter identity)
       (count)))

(def example [199 200 208 210 200 207 240 269 260 263])

(def input
  (->> (util/read-input)
       (str/split-lines)
       (map parse-long)))

(defn part-1
  []
  (count-increasing input))

(defn windowed-sum
  [xs]
  (->> xs
       (partition 3 1)
       (map #(apply + %))))

(defn part-2
  []
  (->> input
       (windowed-sum)
       (count-increasing)))
