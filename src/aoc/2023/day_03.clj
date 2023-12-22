(ns aoc.2023.day-03
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def example "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")
(def input (util/read-input))

(defn parse-input
  [input]
  (vec
    (for [line (str/split-lines input)]
      (vec line))))

(defn symbol?
  [item]
  (re-find #"[^\d\.]" (str item)))

(defn find-symbols
  [grid]
  (->> (apply concat
          (map-indexed
            (fn [y row]
              (map-indexed
                (fn [x item]
                  [item [x y]])
                row))
            grid))
       (filter (fn [[item]]
                 (symbol? item)))))

(find-symbols (parse-input example))
