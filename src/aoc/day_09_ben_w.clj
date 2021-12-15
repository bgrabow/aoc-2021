(ns aoc.day-09-ben-w
  (:require [clojure.string :as str]))

(def example "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")
(def input (slurp "resources/input/day_09.txt"))
(def ben-w-input (slurp "resources/input/day_09_ben_w.txt"))

(comment
  (count (str/split-lines input)))

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (mapv #(mapv parse-long (str/split % #"")))))

(comment
  (let [cave [[2 1 9 9 9 4 3 2 1 0]
              [3 9 8 7 8 9 4 9 2 1]]
        y 1 x 8]
    (get-in cave [y x]))

  ;; O(1) access
  ;; Easy to iterate by row
  ;; Awkward to iterate by column
  ;; Awkward to iterate through k-v pairs
  (let [cave {[0 0] 2 [1 0] 1}]
    (get cave [0 0]))

  ;; O(log-32 n) access
  ;; Awkward to iterate by row or column.
  ;; Easy iterate over all points.
  ;; Easy to iterate through k-v pairs.


  ,)



;; General algorithm
;; Given a point p, examine neighbors ([1 0] [-1 0] [0 1] [0 -1])
;; If all neighbors are higher numbers, it is a low point
;; n.b. Neighbors of low points can't be low points. (Up to 50% work reduction.)

(defn height
  [cave [x y]]
  (get-in cave [y x]))

(comment
  (height [[2 1 9 9 9 4 3 2 1 0] [3 9 8 7 8 9 4 9 2 1] [9 8 5 6 7 8 9 8 9 2] [8 7 6 7 8 9 6 7 8 9] [9 8 9 9 9 6 5 6 7 8]] [0 0]))

(defn neighbors
  [cave [x y]]
  (->> (for [dp [[1 0] [-1 0] [0 1] [0 -1]]
             :let [p' (mapv + [x y] dp)]]
         (do #_(prn p' (height cave p'))
           (height cave p')))
       (remove nil?)))

(comment
  (let [cave (parse-input example)]
    (neighbors cave [0 0])))

(defn is-low-point?
  [cave p]
  (every? #(< (height cave p) %) (neighbors cave p)))

(comment
  (is-low-point? (parse-input example) [0 0])
  (is-low-point? (parse-input example) [1 0])
  (is-low-point? (parse-input example) [5 0])
  (neighbors (parse-input example) [5 0]))

(defn all-points
  [vec-2d]
  (for [x (range (count (first vec-2d)))
        y (range (count vec-2d))]
    [x y]))

(defn part-1
  []
  (let [cave (parse-input ben-w-input)
        points (all-points cave)]
    (->> (filter (partial is-low-point? cave) points)
         (map (partial height cave))
         (map inc)
         (apply +))))

(comment
  (all-points (parse-input example))

  (mapv (partial mapv #(case % 9 9 0)) (parse-input example)))

;; General algorithm
;; Iterate over each low point
;; Perform a flood fill or graph traversal from each low point
;; Count the number of points traversed in each basin

(defn low-points
  [cave]
  (filter (partial is-low-point? cave) (all-points cave)))

(defn basin-neighbors
  [cave [x y]]
  (->> (for [dp [[1 0] [-1 0] [0 1] [0 -1]]
             :let [p' (mapv + [x y] dp)]
             :when (and (height cave p')
                        (not= 9 (height cave p')))]
         p')))

(comment
  (basin-neighbors (parse-input example) [4 0]))

(defn explore-basin
  [cave p0]
  (loop [visited #{p0}
         frontier (basin-neighbors cave p0)]
    (if (seq frontier)
      (recur (conj visited (first frontier))
             (into (next frontier)
                   (->> (basin-neighbors cave (first frontier))
                        (remove visited)
                        (remove (set frontier)))))
      visited)))

(defn part-2
  []
  (time (let [cave (parse-input ben-w-input)]
          (->> (low-points cave)
               (map (partial explore-basin cave))
               (map count)
               (sort-by -)
               (take 3)
               (apply *)))))
