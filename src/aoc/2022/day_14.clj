(ns aoc.2022.day-14
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def input (slurp "resources/2022/input14.txt"))
(def example-1 "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

(defn irange
  [start end]
  (range start (inc end)))

(defn pair->points
  [p]
  (let [[[_ x1 y1] [_ x2 y2]] (sort p)]
    (for [x (range (parse-long x1) (inc (parse-long x2)))
          y (range (parse-long y1) (inc (parse-long y2)))]
      [x y])))

(defn parse-input
  [s]
  {:rock (->> (str/split-lines s)
              (map #(re-seq #"(\d+),(\d+)" %))
              (mapcat #(partition 2 1 %))
              (mapcat pair->points)
              (into #{}))
   :sand #{}
   :paths []})

(def down [0 1])
(def down-left [-1 1])
(def down-right [1 1])

(defn sand-path
  [state start]
  (when start
    (lazy-seq
      (cons start
            (sand-path state (first (->> [down down-left down-right]
                                         (map #(mapv + start %))
                                         (remove (:rock state))
                                         (remove (:sand state)))))))))

(def lbound
  (memoize
    (fn [rocks]
      (->> rocks
           (map second)
           (apply max)))))

(defn into-abyss?
  [state path]
  (some (fn [[_x y]] (>= y (lbound (:rock state)))) path))

(defn drop-sand
  [state]
  (let [initial-path (drop-last 1 (:last-path state))
        path (concat initial-path (rest (sand-path state (or (last initial-path) [500 0]))))]
    (if (into-abyss? state path)
      state
      (-> state
          (update :sand conj (last path))
          #_(update :paths conj path)
          (assoc :last-path path)))))

(defn print-state
  [state]
  (let [xs (->> (concat (:rock state) (:sand state))
                (map first)
                (sort))
        ys (->> (concat (:rock state) (:sand state))
                (map second)
                (sort))
        xlow (first xs)
        xhigh (last xs)
        _ylow (first ys)
        _yhigh (last ys)]
    (for [x (range xlow (inc xhigh))]
      (cond
        (rem x 10) (format "%3d" x)
        (rem x 5) "  |"
        :else "  ."))))

(take 20 (iterate drop-sand (parse-input example-1)))
(into-abyss? (parse-input input) (sand-path (parse-input input) [500 0]))
(->> (util/iterate-until-fixed drop-sand (parse-input input))
     (last)
     :sand
     count)

