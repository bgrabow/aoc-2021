(ns aoc.2022.day-13
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (slurp "resources/2022/input13.txt"))
(def example-1 "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]")

(defn parse-input
  [s]
  (->> (str/split s #"\n\n")
       (map (comp #(map edn/read-string %) str/split-lines))))

(declare in-order?)

(defn lists-in-order?
  [l r]
  (->> (concat (map in-order? l r) [(when (not= (count l) (count r)) (< (count l) (count r)))])
       (remove nil?)
       (first)))

(defn in-order?
  [l r]
  (->> (map deref [(delay (when (and (number? l) (number? r) (not= l r)) (< l r)))
                   (delay (when (and (coll? l) (coll? r)) (lists-in-order? l r)))
                   (delay (when (and (coll? l) (number? r)) (lists-in-order? l [r])))
                   (delay (when (and (number? l) (coll? r)) (lists-in-order? [l] r)))])
       (remove nil?)
       (first)))

(defn solve-1
  [s]
  (->> (map #(apply in-order? %) (parse-input s))
       (keep-indexed (fn [i x]
                       (when x (inc i))))
       (reduce +)))

(comment
  (solve-1 input))

(defn solve-2
  [s]
  (let [packets (apply concat (parse-input s))
        packets-below-2 (filter #(in-order? % [[2]]) packets)
        packets-above-6 (filter #(in-order? [[6]] %) packets)
        index-2 (inc (count packets-below-2))
        index-6 (- (+ (count packets) 2) (count packets-above-6))]
    (* index-2
       index-6)))

(comment
  (solve-2 input))
