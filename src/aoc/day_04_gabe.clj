(ns aoc.day-04-gabe
  (:require [clojure.string :as str]
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

(def input (slurp "resources/input/day_04.txt"))

(defn parse-row
  [s]
  (str/split (str/trim s) #"\s+"))

(defn parse-board
  [s]
  (->> (str/split-lines s)
       (mapv parse-row)))

(defn parse-input
  [input]
  (let [[balls & boards] (str/split input #"\n\n")]
    {:balls  (str/split balls #",")
     :boards (map parse-board boards)}))

(defn mark-board
  [board ball]
  (mapv (fn [row]
          (mapv #(if (= ball %)
                   "x" %)
                row))
        board))

(let [ball "7"]
 (mapv (fn [row]
         (mapv #(if (= ball %)
                  "x" %)
               row))
       [["19" "8" "7" "25" "23"]]))

(mark-board
  [["3" "15" "0" "2" "22"]
   ["9" "18" "13" "17" "5"]
   ["19" "8" "7" "25" "23"]
   ["20" "11" "10" "24" "4"]
   ["14" "21" "16" "12" "6"]]
  "7")

(defn mark-boards
  [boards ball]
  (map #(mark-board % ball) boards))

(defn columns
  [matrix]
  (apply map vector matrix))

(defn rows
  [matrix]
  matrix)

(defn all-x?
  [coll]
  (every? #(= "x" %) coll))

(defn is-winner?
  [board]
  (or (some all-x? (columns board))
      (some all-x? (rows board))))

(defn has-winner?
  [boards]
  (some is-winner? boards))

(defn winners
  [boards]
  (seq (filter is-winner? boards)))

(defn non-winners
  [boards]
  (seq (remove is-winner? boards)))

(defn score-board
  [[boards ball]]
  (->> (flatten boards)
       (remove #{"x"})
       (map parse-long)
       (apply +)
       (* (parse-long ball))))

(def gabe's-input (slurp "resources/input/gabe_day_04.txt"))

(defn part-1
  []
  (let [{:keys [boards balls]} (parse-input gabe's-input)]
    (->> (map vector (drop 1 (reductions mark-boards boards balls)) balls)
         (keep (fn [[boards ball]]
                 (some-> (winners boards)
                         (vector ball))))
         (first)
         (score-board))))

(defn score-finishing-transition
  [[second-last-state last-state]]
  (let [second-last-boards (first second-last-state)
        winning-ball (second last-state)]
    (score-board [[(mark-board (first second-last-boards) winning-ball)] winning-ball])))

(defn part-2
  []
  ;; Find board that wins last
  ;; *
  ;; Score that board
  #_(let [{:keys [boards balls]} (parse-input example-input)
          n-boards (count boards)]
      (->> (map vector (drop 1 (reductions mark-boards boards balls)) balls)
           (map (fn [[boards ball]]
                  [(winners boards) ball]))
           (partition 2 1)
           (filter (fn [[old new]]
                     (= [(dec n-boards) n-boards]
                        [(count (first old)) (count (first new))])))
           (first)))

  (let [{:keys [boards balls]} (parse-input gabe's-input)]
    (->> (map vector (drop 1 (reductions mark-boards boards balls)) balls)
         (map (fn [[boards ball]]
                [(non-winners boards) ball]))
         (partition 2 1)
         (filter (fn [[_old new]]
                   (zero? (count (first new)))))
         (first)
         (score-finishing-transition)))
  ,)
