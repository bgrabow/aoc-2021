(ns aoc.2022.day-06
  (:require [clojure.string :as str]))

(def input (slurp "resources/2022/input06.txt"))

(defn index-of-som
  [width s]
  (->> (partition width 1 s)
       (map set)
       (take-while #(< (count %) width))
       (count)
       (+ width)))

(defn solve-1
  [s]
  (index-of-som 4 (first (str/split-lines s))))

(comment
  (solve-1 input))

(defn solve-2
  [s]
  (index-of-som 14 (first (str/split-lines s))))

(comment
  (solve-2 input))
