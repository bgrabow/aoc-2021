(ns aoc.day-09
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def example-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n")

(def input (util/read-input))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (mapv (partial mapv (comp parse-long str)))))

(defn lookup
  [mat p]
  (get-in mat (reverse p)))

(defn neighbors
  [mat p]
  (filter identity
          (for [dp [[-1 0] [1 0] [0 -1] [0 1]]
                :let [p' (mapv + p dp)]]
            (when (lookup mat p') p'))))

(defn local-minima?
  [mat p]
  (let [[p-height & neighbors-height] (map (partial lookup mat) (cons p (neighbors mat p)))]
    (every? #(< p-height %) neighbors-height)))

(defn risk-level
  [mat p]
  (inc (lookup mat p)))

(defn low-points
  [mat]
  (->> (for [x (range (count (first mat)))
             y (range (count mat))]
         [x y])
       (filter (partial local-minima? mat))))

(defn part-1
  []
  (let [matrix (parse-input input)]
    (->> (low-points matrix)
         (map (partial risk-level matrix))
         (reduce +))))

(defn explore-basin
  [mat acc]
  (let [{:keys [explored frontier]} acc]
    (if (seq frontier)
      (let [p (first frontier)
            new-basin-neighbors (->> (neighbors mat p)
                                     (remove #(= 9 (lookup mat %)))
                                     (remove explored))]
        {:explored (apply conj explored new-basin-neighbors)
         :frontier (apply conj (disj frontier p) new-basin-neighbors)}))))

(defn basin
  [mat p]
  (->> (iterate
         (partial explore-basin mat)
         {:explored #{p}
          :frontier #{p}})
       (drop-while #(seq (:frontier %)))
       (first)
       :explored))

(defn part-2
  []
  (time (let [matrix (parse-input input)
              low-points (low-points matrix)]
          (->> (map (partial basin matrix) low-points)
               (map count)
               (sort-by -)
               (take 3)
               (reduce *)))))

(comment
  (let [mat (parse-input example-input)
        p [9 0]]
    (basin mat p)))
