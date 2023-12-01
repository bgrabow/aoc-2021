(ns aoc.2023.day-01
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def input (util/read-input))

(def spelled-digits
  (zipmap ["one"
           "two"
           "three"
           "four"
           "five"
           "six"
           "seven"
           "eight"
           "nine"]
          (range 1 10)))

(defn part-1
  [input]
  (reduce
    +
    (for [line (str/split-lines input)]
      (let [digits (re-seq #"\d" line)]
        (parse-long (str (first digits) (last digits)))))))

(def digits-or-spelled-digits-re
  (re-pattern
    (format
      "(?=(\\d))|%s"
      (str/join "|"
                (map (fn [digit-name]
                       (format "(?=(%s))" digit-name))
                     (keys spelled-digits))))))

(defn part-2
  [input]
  (reduce
    +
    (for [line (str/split-lines input)]
      (let [digits (->> (re-seq digits-or-spelled-digits-re line)
                        (map #(second (remove nil? %)))
                        (map #(or (spelled-digits %) %)))]
        (doto (parse-long (str (first digits) (last digits)))
          (println line))))))
