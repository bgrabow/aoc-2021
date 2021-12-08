(ns aoc.day-07
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def example-input "16,1,2,0,4,2,7,1,2,14")

(def input (util/read-input))

(defn parse-input
  [input]
  (map parse-long (str/split (str/trim input) #",")))

(defn mean
  [xs]
  (/ (reduce + xs) (count xs)))

(defn fuel-required
  [crabs destination]
  (->> (map (comp #(Math/abs ^long %) #(- % destination)) crabs)
       (reduce +)))

(defn exhaustive-search
  "Given a fitness function, f, and a set of input values, xs, exhaustively
  search for the x such that (f x) is a minimum."
  [f xs]
  (->> (map (juxt identity f) xs)
       (sort-by second)
       (ffirst)))

(defn descend-kvs
  [kvs]
  (->> (partition 2 1 kvs)
       (drop-while #(>= (second (first %)) (second (second %))))
       (ffirst)))

(defn gradient-descent
  "Given a fitness function, f, and a starting position, x0 an integer, find
  the tuple, [x (f x)], that minimizes (f x) within the optimization valley
  containing x0."
  [f x0]
  (assert (zero? (rem x0 1)))
  (let [dx (range)
        leftward (map #(- x0 %) dx)
        rightward (map #(+ x0 %) dx)
        least-left (->> (map f leftward)
                        (map vector leftward)
                        (descend-kvs))
        least-right (->> (map f rightward)
                         (map vector rightward)
                         (descend-kvs))]
    (->> (sort-by second [least-left least-right])
         (first))))

(defn part-1
  []
  (let [crabs (parse-input input)
        ideal-position (mean crabs)]
    (fuel-required crabs ideal-position)
    (map vector (range) (map #(fuel-required crabs %) (range 17)))
    (fuel-required crabs (exhaustive-search
                           (partial fuel-required crabs)
                           (range (apply min crabs)
                                  (inc (apply max crabs)))))))

(defn median
  [xs]
  (let [n (count xs)]
    (if (even? n)
      (mean (take 2 (drop (dec (quot n 2)) (sort xs))))
      (nth (sort xs) (quot n 2)))))

(comment
  (time (let [crabs (parse-input input)]
          (gradient-descent
            (partial fuel-required crabs)
            (median crabs))))
  (time (let [crabs (parse-input input)]
          (gradient-descent
            (partial fuel-required crabs)
            0))))

(defn triangular-fuel-cost
  [dx]
  (/ (* dx (inc dx)) 2))

(defn triangular-fuel-required
  [crabs destination]
  (->> (map (comp triangular-fuel-cost #(Math/abs ^long (- % destination))) crabs)
       (reduce +)))

(defn part-2
  []
  (time (let [crabs (parse-input input)]
          (second (gradient-descent
                    (partial triangular-fuel-required crabs)
                    (int (mean crabs)))))))

(comment
  (time (let [crabs (parse-input input)]
          (gradient-descent
            (partial triangular-fuel-required crabs)
            0)))
  (time (let [crabs (parse-input input)]
          (triangular-fuel-required crabs 0)))
  (time (let [crabs (parse-input input)]
          (gradient-descent
            (partial triangular-fuel-required crabs)
            (int (mean crabs))))))

(comment
  (let [crabs (parse-input example-input)]
    (map (partial triangular-fuel-required crabs) (range 17)))
  (mean (parse-input input))
  (triangular-fuel-required (parse-input input) 489)
  (triangular-fuel-required (parse-input example-input) (Math/round ^double (mean (parse-input example-input))))
  (sort-by first (map (fn [[k v]] [k (count v)]) (group-by #(count (str %)) (parse-input input)))))
