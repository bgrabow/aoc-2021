(ns aoc.day-15
  (:require [clojure.string :as str])
  (:import (java.util PriorityQueue)))

(def example "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")
(def start [0 0])

(defn parse-grid
  [s]
  (->> (str/split-lines s)
       (mapv #(mapv (comp parse-long str) %))))

(defn lookup
  [matrix p]
  (get-in matrix (reverse p)))

(defn a-star
  [matrix start]
  (-> (PriorityQueue.
        (reduce + (map count matrix))
        #(compare (lookup matrix %1) (lookup matrix %2)))
      (.add start)))

(a-star (parse-grid example) start)
(lookup (parse-grid example) start)
