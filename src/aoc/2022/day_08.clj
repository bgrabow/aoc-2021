(ns aoc.2022.day-08
  (:require [clojure.string :as str]))

(def input (slurp "resources/2022/input08.txt"))

(defn parse-input
  [s]
  (mapv #(mapv (comp parse-long str) %) (str/split-lines s)))

(defn tree-at
  [forest pos]
  (get-in forest (reverse pos)))

(def largest-tree-in-dir
  (memoize
    (fn [forest pos dir]
      (if-let [tree (tree-at forest pos)]
        (if-let [neighbor (largest-tree-in-dir forest (mapv + pos dir) dir)]
          (max neighbor tree)
          tree)
        nil))))

(def example-1 "30373\n25512\n65332\n33549\n35390")

(defn visible-from-dir?
  [forest pos dir]
  (let [neighbor (largest-tree-in-dir forest (mapv + pos dir) dir)]
    (println "visible-from-dir?" (zipmap [:neighbor :pos :dir :tree] [neighbor pos dir (tree-at forest pos)]))
    (if neighbor
      (> (tree-at forest pos) neighbor)
      true)))

(def unit-vecs [[0 -1] [0 1] [-1 0] [1 0]])

(defn visible-from-outside-grid?
  [forest pos]
  (some (partial visible-from-dir? forest pos) unit-vecs))

(defn positions
  [mat]
  (for [y (range (count mat))
        x (range (count (get mat y)))]
    [x y]))

(defn solve-1
  [s]
  (->> (positions (parse-input s))
       (filter (partial visible-from-outside-grid? (parse-input s)))
       (count)))

(comment
  (->> (positions (parse-input example-1))
       (filter (partial visible-from-outside-grid? (parse-input example-1))))
  (solve-1 input)
  (visible-from-outside-grid? (parse-input input) [0 2]))

(defn viewing-distance
  ([forest pos dir]
   (viewing-distance forest (mapv + pos dir) dir (tree-at forest pos)))
  ([forest pos dir height]
   (doto
     (if (tree-at forest pos)
       (if (> height (tree-at forest pos))
         (inc (viewing-distance forest (mapv + pos dir) dir height))
         1)
       0)
     #_(#(println "viewing-distance" {:pos pos :dir dir :tree (tree-at forest pos) :heigh height :ret %})))))

(defn viewing-distances
  [forest pos]
  (map (partial viewing-distance forest pos) unit-vecs))

(defn scenic-score
  [forest pos]
  (apply * (viewing-distances forest pos)))

(defn solve-2
  [s]
  (let [forest (parse-input s)]
    (->> (positions forest)
         (map (partial scenic-score forest))
         (apply max))))

(comment
  (solve-2 input))
