(ns aoc.day-06
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def input (util/read-input))

(def example-input "3,4,3,1,2")

(defn parse-input
  [input]
  (mapv (comp parse-long str/trim) (str/split input #",")))

(defn advance-day
  [fish]
  (->> fish
       (mapcat #(if (zero? %)
                  [6 8]
                  [(dec %)]))))

(defn part-1
  []
  (time (->> (iterate advance-day (parse-input input))
             (drop 80)
             (first)
             (count))))

(frequencies (parse-input input))

(defn advance-fish-count
  [fish-freq]
  (apply merge-with + (map (fn [[days fish-count]]
                               (if (> days 0)
                                 {(dec days) fish-count}
                                 {6 fish-count 8 fish-count}))
                           fish-freq)))

(defn part-1-faster
  []
  (time (->> (iterate advance-fish-count (frequencies (parse-input input)))
             (drop 80)
             (first)
             (vals)
             (reduce +))))

(defn map-keys
  [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn advance-week
  [fish-freq]
  (let [spawning (select-keys fish-freq (range 7))
        new (select-keys fish-freq [7 8])]
    (merge-with +
                spawning
                (map-keys #(- % 7) new)
                (map-keys #(+ 2 %) spawning))))

(defn advance-fish-count-n
  [n-days fish-freq]
  (let [n-cycles (quot n-days 7)
        remainder (rem n-days 7)]
    (println n-cycles remainder)
    (->> fish-freq
         (iterate advance-week)
         (drop n-cycles)
         (first)
         (iterate advance-fish-count)
         (drop remainder)
         (first))))

(defn part-2-faster
  []
  (time (->> (iterate advance-fish-count (frequencies (parse-input input)))
             (drop 256)
             (first)
             (vals)
             (reduce +))))

(defn part-1-fastest
  []
  (time (->> (advance-fish-count-n 80 (frequencies (parse-input input)))
             (vals)
             (reduce +))))

(defn part-2-fastest
  []
  (time (->> (advance-fish-count-n 256 (frequencies (parse-input input)))
             (vals)
             (reduce +))))
