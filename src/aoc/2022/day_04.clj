(ns aoc.2022.day-04
  (:require [clojure.string :as str]))

(def input (slurp "resources/2022/input04.txt"))

(defn parse-pair
  [s]
  (zipmap [:a-low :a-high :b-low :b-high] (map parse-long (re-seq #"\d+" s))))

(defn contained-pair?
  [p]
  (or (and (<= (:a-low p) (:b-low p))
           (>= (:a-high p) (:b-high p)))
      (and (>= (:a-low p) (:b-low p))
           (<= (:a-high p) (:b-high p)))))

(defn overlap?
  [p]
  (or (<= (:a-low p) (:b-low p) (:a-high p))
      (<= (:b-low p) (:a-low p) (:b-high p))))

(defn solve-1
  [s]
  (->> (str/split-lines s)
       (map parse-pair)
       (filter contained-pair?)
       (count)))

(solve-1 input)

(defn solve-2
  [s]
  (->> (str/split-lines s)
       (map parse-pair)
       (filter overlap?)
       (count)))

(solve-2 input)
