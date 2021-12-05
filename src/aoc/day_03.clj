(ns aoc.day-03
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def input (->> (util/read-input)
                (str/split-lines)
                (map #(mapv (comp parse-long str) %))))

(def example-input
  (->> "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"
       (str/split-lines)
       (map #(mapv (comp parse-long str) %))))

(defn columns
  [input]
  (apply map vector input))

(defn freqs
  [columns]
  (map frequencies columns))

(defn binary-array->long
  [binary-array]
  (reduce (fn [acc digit] (+ digit (* acc 2))) 0 binary-array))

(defn part-1
  []
  (let [[gamma epsilon] (->> (map #(sort-by second %) (freqs (columns input)))
                             ((juxt #(map ffirst %) #(map (comp first second) %)))
                             (map binary-array->long))]
    (* gamma epsilon)))

(defn most-common-digit
  [xs]
  (->> xs
       (frequencies)
       (sort-by (juxt second first))
       last
       first))

(defn least-common-digit
  [xs]
  (->> xs
       (frequencies)
       (sort-by (juxt second first))
       first
       first))

(defn oxygen-generator-rating
  [diagnostic-report-rows]
  (first (reduce
           (fn [rows place]
             (let [mcd (->> rows
                            (map #(nth % place))
                            most-common-digit)]
               (filter #(= mcd (nth % place)) rows)))
           diagnostic-report-rows
           (range (count (first diagnostic-report-rows))))))

(defn co2-generator-rating
  [diagnostic-report-rows]
  (first (reduce
           (fn [rows place]
             (let [lcd (->> rows
                            (map #(nth % place))
                            least-common-digit)]
               (filter #(= lcd (nth % place)) rows)))
           diagnostic-report-rows
           (range (count (first diagnostic-report-rows))))))

(comment
  (prn (oxygen-generator-rating input))
  (prn (oxygen-generator-rating example-input))
  (prn (co2-generator-rating input))
  (prn (co2-generator-rating example-input)))

(defn part-2
  []
  (let [oxy-rating (binary-array->long (oxygen-generator-rating input))
        co2-rating (binary-array->long (co2-generator-rating input))]
    (* oxy-rating co2-rating)))

(defn extract-trie
  [rows]
  (when (some seq rows)
    (->> rows
         (group-by first)
         (sort-by (juxt (comp count second) first))
         (map (fn [[d rows]] [d (extract-trie (map next rows))])))))

(defn leftmost-traversal
  [trie]
  (let [[node-value children] (first trie)]
    (when node-value
      (lazy-seq (cons node-value (leftmost-traversal children))))))

(defn rightmost-traversal
  [trie]
  (let [[node-value children] (last trie)]
    (when node-value
      (lazy-seq (cons node-value (rightmost-traversal children))))))

(defn o2-generator-rating-trie
  [input]
  (leftmost-traversal (extract-trie input)))

(defn co2-generator-rating-trie
  [input]
  (rightmost-traversal (extract-trie input)))

(defn part-2-trie
  []
  (let [oxy-rating (binary-array->long (o2-generator-rating-trie input))
        co2-rating (binary-array->long (co2-generator-rating-trie input))]
    (* oxy-rating co2-rating)))

(comment
  (->> input
       (group-by first)
       (sort-by (juxt (comp count second) first))
       (map (fn [[d rows]] [d (map next rows)])))
  (leftmost-traversal (extract-trie example-input))
  (rightmost-traversal (extract-trie example-input))
  (oxygen-generator-rating example-input))
