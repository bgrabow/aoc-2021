(ns aoc.2023.day-10
  (:require [aoc.util :as util]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input (util/read-input))
(def example "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ")
(def example-2 "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n...........")
(comment
  (print example))

(defn start
  [grid]
  (first
    (for [[p c] grid
          :when (= c \S)]
      p)))

(defn valid-neighbors
  [grid p]
  (set
    (for [[d pipes origins] [[[1 0] #{\- \J \7} #{\- \F \L}]
                             [[-1 0] #{\- \F \L} #{\- \J \7}]
                             [[0 1] #{\| \J \L} #{\| \F \7}]
                             [[0 -1] #{\| \F \7} #{\| \J \L}]]
          :let [p' (mapv + p d)]
          :when (and (contains? pipes (grid p'))
                  (or (contains? origins (grid p))
                    (= \S (grid p))))]
      p')))

(defn pipe-path
  [grid]
  (let [s (start grid)]
    (:path
      (last
        (util/iterate-until-fixed
          (fn [{:keys [head included path] :as state}]
            (if-let [neighbor (first (set/difference
                                       (valid-neighbors grid head)
                                       included))]
              {:head neighbor
               :included (conj included neighbor)
               :path (conj path neighbor)}
              state))
          {:head s
           :included #{s}
           :path [s]})))))

(defn part-1
  [input]
  (let [grid (util/parse-2d-grid
               (fn [p c]
                 [p c])
               input)
        path (pipe-path grid)]
    (/ (count path) 2)))

(defn part-2
  [input]
  (let [grid (util/parse-2d-grid
               (fn [p c]
                 [p c])
               input)
        border (set (pipe-path grid))
        y-range (range 0 (inc (count (str/split-lines input))))
        x-range (range 0 (inc (count (first (str/split-lines input)))))]
    (->> (for [y y-range]
           (reduce
             (fn [acc p]
               (cond
                 (and (contains? border p)
                   (contains? #{\| \J \L} (grid p)))
                 (update acc :state {:outside :inside
                                     :inside :outside})

                 (and (not (contains? border p))
                   (= :inside (:state acc)))
                 (update acc :enclosed conj p)

                 :default acc))
             {:state :outside
              :enclosed #{}}
             (mapv vector x-range (repeat y))))
      (map (comp count :enclosed))
      (reduce +))))
