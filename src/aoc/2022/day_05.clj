(ns aoc.2022.day-05
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def input (slurp "resources/2022/input05.txt"))

(defn parse-moves
  [s]
  (->> (str/split-lines s)
       (remove empty?)
       (map #(zipmap [:n :origin :destination] (map parse-long (re-seq #"\d+" %))))))

(defn move-1
  [origin destination stacks]
  (let [origin (dec origin)
        destination (dec destination)]
    (-> stacks
        (update origin pop)
        (update destination conj (peek (get stacks origin))))))

(defn drop-last-vec
  [n v]
  (subvec v 0 (- (count v) n)))

(defn move-n
  [stacks {:keys [n origin destination]}]
  (let [origin (dec origin)
        destination (dec destination)]
    (-> stacks
        (update origin #(drop-last-vec n %))
        (update destination #(reduce conj % (take-last n (get stacks origin)))))))

(comment
  (move-1
    2 3 [["T" "D" "W" "Z" "V" "P"]
         ["L" "S" "W" "V" "F" "J" "D"]
         ["Z" "M" "L" "S" "V" "T" "B" "H"]
         ["R" "S" "J"]
         ["C" "Z" "B" "G" "F" "M" "L" "W"]
         ["Q" "W" "V" "H" "Z" "R" "G" "B"]
         ["V" "J" "P" "C" "B" "D" "N"]
         ["P" "T" "B" "Q"]]))

(defn move
  [stacks {:keys [n origin destination]}]
  (->> (iterate (partial move-1 origin destination) stacks)
       (drop n)
       (first)))

(defn parse-stacks
  [s]
  (->> (str/split-lines s)
       (remove str/blank?)
       (map #(for [crate (partition-all 4 %)]
               (re-find #"\w" (apply str crate))))
       (filter seq)
       util/transpose
       (mapv (comp vec #(remove nil? %) reverse))))

(defn parse-input
  [s]
  (let [[stacks moves] (str/split s #"1   2.+\n")
        stacks (parse-stacks stacks)
        moves (parse-moves moves)]
    [stacks moves]))

(defn solve-1
  [s]
  (->> (apply reduce move (parse-input s))
       (map peek)
       (apply str)))

(comment
  (solve-1 input))

(defn solve-2
  [s]
  (->> (apply reduce move-n (update (parse-input s) 1 #(take 2 %)))
       (map peek)
       (apply str)))

(comment
  (solve-2 input))
