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
  (loop [rows diagnostic-report-rows
         digits []]
    (if (not-any? seq rows)
      digits
      (let [mcd (->> rows
                     (map first)
                     most-common-digit)]
        (recur (->> rows
                    (filter (comp #{mcd} first))
                    (map next))
               (conj digits mcd))))))

(defn co2-generator-rating
  [diagnostic-report-rows]
  (loop [rows diagnostic-report-rows
         digits []]
    (if (not-any? seq rows)
      digits
      (let [lcd (->> rows
                     (map first)
                     least-common-digit)]
        (recur (->> rows
                    (filter (comp #{lcd} first))
                    (map next))
               (conj digits lcd))))))

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
