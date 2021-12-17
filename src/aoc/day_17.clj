(ns aoc.day-17
  (:require [aoc.util :as util]))

(def sample "target area: x=20..30, y=-10..-5")
(def input (util/read-input))

(defn parse-input
  [input]
  (let [[xmin xmax ymin ymax] (map parse-long (re-seq #"\d+" input))]
    {:xmin xmin :xmax xmax :ymin ymin :ymax ymax}))

(defn step-x
  [[x vx]]
  (if (zero? vx) [x vx] [(+ x vx) (dec vx)]))

(defn take-until-fixed
  [coll]
  (reductions #(if (= %1 %2)
                 (reduced %1)
                 %2)
              (first coll) (rest coll)))

(defn in-range-x?
  [])

(take-until-fixed (iterate step-x [0 10]))

