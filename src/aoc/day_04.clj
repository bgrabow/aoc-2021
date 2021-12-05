(ns aoc.day-04
  (:require [clojure.test :as t]
            [clojure.string :as str]
            [aoc.util :as util]
            [clojure.set :as set]))

(def example-input
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

 22 13 17 11  0
  8  2 23  4 24
 21  9 14 16  7
  6 10  3 18  5
  1 12 20 15 19

  3 15  0  2 22
  9 18 13 17  5
 19  8  7 25 23
 20 11 10 24  4
 14 21 16 12  6

 14 21 17 24  4
 10 16 15  9 19
 18  8 23 26 20
 22 11 13  6  5
  2  0 12  3  7")

(def input (util/read-input))

(defn transpose
  [colls]
  (apply map vector colls))

(defn parse-board
  [board-str]
  (let [xs (map parse-long
                (str/split
                  (str/trim
                    (str/join \space board-str))
                  #"\s+"))
        rows (partition 5 xs)
        cols (transpose rows)
        _tlbr-diag (flatten (partition 1 6 xs))
        _trbl-diag (flatten (drop-last 1 (drop 1 (partition 1 4 xs))))]
    {:all (set xs)
     :win-lanes (map set (concat rows cols))}))

(defn parse-input
  [input]
  (let [[balls & boards] (str/split-lines input)
        balls (mapv parse-long (str/split balls #","))
        boards (->> boards
                    (partition-by empty?)
                    (remove (comp empty? first))
                    (map parse-board))]
    [balls boards]))

(defn winners
  [boards balls]
  (let [balls (set balls)]
    (->> boards
         (filter (fn [board]
                   (->> (:win-lanes board)
                        (map (partial #(set/difference % balls)))
                        (some empty?))))
         seq)))

(defn score
  [board balls]
  (let [unmarked (set/difference (:all board) (set balls))]
    (* (reduce + unmarked)
       (last balls))))

(defn score-game
  [input]
  (let [[balls boards] (parse-input input)
        winning-balls (->> balls
                           (reductions conj [])
                           (filter (partial winners boards))
                           first)
        winning-board (first (winners boards winning-balls))]
    (score winning-board winning-balls)))

(t/deftest example
  (t/is (= 4512 (score-game example-input))))

(defn part-1
  []
  (score-game input))

(defn last-winner
  [input]
  (let [[balls boards] (parse-input input)
        all-won-balls (->> balls
                           (reductions conj [])
                           (drop-while #(> (count boards)
                                           (count (winners boards %))))
                           first)
        last-winner (first (set/difference
                             (set boards)
                             (set (winners boards (butlast all-won-balls)))))]
    (score last-winner all-won-balls)))

(defn part-2
  []
  (last-winner input))
