(ns aoc.day-14
  (:require [clojure.string :as str]))

(def example "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")

(defn parse-input
  [input]
  (let [[polymer insertion-rules] (str/split input #"\n\n")]
    [polymer
     (->> (str/split-lines insertion-rules)
          (map #(vec (re-seq #"\w+" %)))
          (into {}))]))

(defn polymerize
  [insertion-rules polymer]
  (->> (partition 2 1 polymer)
       (map #(apply str %))
       (map (comp #(apply str %) (juxt first insertion-rules last)))
       (reduce #(str (apply str (butlast %1)) %2) "")))

(comment
  ;; Example
  (prn
    (= (let [[polymer insertion-rules] (parse-input example)]
         (->> (iterate (partial polymerize insertion-rules) polymer)
              (take 5)))
       ["NNCB"
        "NCNBCHB"
        "NBCCNBBBCBHCB"
        "NBBBCNCCNBBNBNBBCHBHHBCHB"
        "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"])))
