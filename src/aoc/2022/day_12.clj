(ns aoc.2022.day-12
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def input (slurp "resources/2022/input12.txt"))

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (mapv vec)
       (util/transpose)))

(defn height
  [heightmap pos]
  (int
    (let [c (get-in heightmap pos)]
      (cond
        (= \S c) \a
        (= \E c) \z
        :else c))))

(defn neighbors
  [heightmap pos]
  (->> (for [d [[1 0] [0 1] [-1 0] [0 -1]]]
         (mapv + d pos))
       (filter #(get-in heightmap %))
       (filter #(<= (height heightmap %) (inc (height heightmap pos))))))

(defn neighbors-down
  [heightmap pos]
  (->> (for [d [[1 0] [0 1] [-1 0] [0 -1]]]
         (mapv + d pos))
       (filter #(get-in heightmap %))
       (filter #(>= (height heightmap %) (dec (height heightmap pos))))))

(defn find-in-mat
  [matrix needle]
  (first (for [x (range (count matrix))
               y (range (count (first matrix)))
               :when (= needle (get-in matrix [x y]))]
           [x y])))

(defn find-all
  [matrix needle]
  (for [x (range (count matrix))
        y (range (count (first matrix)))
        :when (= needle (get-in matrix [x y]))]
    [x y]))

(defn solve-1
  [s]
  (let [heightmap (parse-input s)
        destination (find-in-mat heightmap \E)
        origin (find-in-mat heightmap \S)]
    (-> (util/dijkstra
          (partial neighbors heightmap)
          (constantly 1)
          origin
          destination)
        (get-in [:visited destination]))))

(comment
  (solve-1 input))

(defn solve-2
  [s]
  (let [heightmap (parse-input s)
        destination (fn [pos] (#{\a \S} (get-in heightmap pos)))
        origin (find-in-mat heightmap \E)]
    (-> (util/dijkstra-2
          (partial neighbors-down heightmap)
          (constantly 1)
          origin
          destination)
        (:visited)
        (->> (filter (comp destination first)))))
  #_(let [heightmap (parse-input s)
          destination (find-in-mat heightmap \E)]
      (->> (for [origin (find-all heightmap \a)]
             (-> (util/dijkstra
                   (partial neighbors heightmap)
                   (constantly 1)
                   origin
                   destination)
                 (get-in [:visited destination])))
           (remove nil?)
           (apply min))))

(comment
  (solve-2 input))
