(ns aoc.2024.day-02
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(defn gradually-increasing?
  [xs]
  (every? #(<= 1 % 3) (map - (rest xs) xs)))

(defn gradually-decreasing?
  [xs]
  (every? #(<= 1 % 3) (map - xs (rest xs))))

(defn valid-level?
  [xs]
  (or (gradually-increasing? xs)
    (gradually-decreasing? xs)))

(defn part-1
  []
  (count
    (filter identity
      (for [line (str/split-lines (util/read-input))]
        (let [level (map parse-long (re-seq #"\d+" line))]
          (valid-level? level))))))
