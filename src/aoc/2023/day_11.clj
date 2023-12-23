(ns aoc.2023.day-11
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def input (util/read-input))

(defn double-rows
  [rows]
  (mapcat
    (fn [row]
      (if (every? #{\.} row)
        [row row]
        [row]))
    rows))

(defn transpose
  [rows]
  (apply map str rows))

(defn total-pairwise-distance
  [galaxies]
  (/ (->> (for [p galaxies
                q galaxies
                :when (not= p q)]
            (->> (map - p q)
              (map #(Math/abs ^long %))
              (reduce +)))
       (reduce +)) 2))

(defn part-1
  [input]
  (let [galaxies (->> (str/split-lines input)
                   double-rows
                   transpose
                   double-rows
                   transpose
                   (str/join \newline)
                   (util/parse-2d-grid
                     (fn [p c]
                       (when (= \# c)
                         [p c])))
                   (keys))]
    (total-pairwise-distance galaxies)))

(comment
  (let [x-range (range 0 (apply max (map first galaxies)))
        y-range (range 0 (apply max (map second galaxies)))]))

(defn empty-rows
  [rows]
  (keep-indexed
    (fn [y row]
      (when (every? #{\.} row)
        y))
    rows))

(defn empty-cols
  [rows]
  (empty-rows (transpose rows)))

(defn expand-galaxy
  [empty-rows empty-cols [x y]]
  [(+ x (* (dec 1000000) (count (filter #(< % x) empty-cols))))
   (+ y (* (dec 1000000) (count (filter #(< % y) empty-rows))))])

(defn part-2
  [input]
  (let [rows (str/split-lines input)
        empty-rows (empty-rows rows)
        empty-cols (empty-cols rows)
        galaxies (->> rows
                   (str/join \newline)
                   (util/parse-2d-grid
                     (fn [p c]
                       (when (= \# c)
                         [p c])))
                   (keys)
                   (map (fn [p]
                          (expand-galaxy empty-rows empty-cols p))))]
    (total-pairwise-distance galaxies)))
