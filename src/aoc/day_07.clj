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

(def triangular-fuel-cost
  (memoize
    (fn [dx]
      (let [dx (Math/abs ^long dx)]
        (case dx
          0 0
          1 1
          (+ dx (triangular-fuel-cost (dec dx))))))))

(defn triangular-fuel-required
  [crabs destination]
  (->> (map (comp triangular-fuel-cost #(Math/abs ^long (- % destination))) crabs)
       (reduce +)))

(defn part-2
  []
  (time (let [crabs (parse-input input)]
          (triangular-fuel-required
            crabs
            (exhaustive-search
              (partial triangular-fuel-required crabs)
              (range (apply min crabs)
                     (inc (apply max crabs))))))))

(comment
  (let [crabs (parse-input example-input)]
    (map (partial triangular-fuel-required crabs) (range 17)))
  (mean (parse-input input))
  (triangular-fuel-required (parse-input input) 489)
  (triangular-fuel-required (parse-input example-input) (Math/round ^double (mean (parse-input example-input))))
  (sort-by first (map (fn [[k v]] [k (count v)]) (group-by #(count (str %)) (parse-input input)))))
