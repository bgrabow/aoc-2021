(ns aoc.2023.day-02
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def example "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
(def input (util/read-input))

(def part-1-constraint
  {:blue 14
   :red 12
   :green 13})

(defn parse-input
  [input]
  (for [game (str/split-lines input)]
    (let [game-n (parse-long (second (re-find #"Game (\d+):" game)))
          pulls (drop 1 (re-seq #"[^;:\n]+" game))]
      {:game game-n
       :pulls (vec
                (for [pull pulls]
                  (apply merge
                         (for [[_ n color] (re-seq #"(\d+) (\w+)" pull)]
                           {(keyword color) (parse-long n)}))))})))

(defn subset?
  [x basis]
  (every? #(<= (second %) (get basis (first %))) x))

(defn part-1
  [input]
  (->> (parse-input input)
       (filter #(subset? (apply merge-with max (:pulls %)) part-1-constraint))
       (map :game)
       (reduce +)))

(defn power
  [cube-set]
  (apply * (map cube-set [:red :green :blue])))

(defn part-2
  [input]
  (->> (parse-input input)
       (map #(apply merge-with max (:pulls %)))
       (map #(assoc % :power (power %)))
       (map :power)
       (reduce +)))
