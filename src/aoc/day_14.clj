(ns aoc.day-14
  (:require [clojure.string :as str]
            [aoc.util :as util]
            [clojure.test :as t]))

(def example "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")
(def input (util/read-input))

(defn insertion
  [insertion-rules pair]
  (get insertion-rules pair ""))

(defn parse-input
  [input]
  (let [[polymer insertion-rules] (str/split input #"\n\n")]
    [polymer
     (->> (str/split-lines insertion-rules)
          (map #(vec (re-seq #"\w+" %)))
          (into {}))]))

(defn polymerize
  [insertion-rules polymer]
  (->> (partition-all 2 1 polymer)
       (map #(apply str %))
       (map #(apply str (interpose (insertion insertion-rules %) %)))
       (reduce #(str (apply str (butlast %1)) %2) "")))

(defn next-generation-pairs
  [insertion-rules pair]
  (->> (apply str (first pair) (insertion insertion-rules (apply str pair)) (second pair))
       (partition-all 2 1)
       (map #(apply str %))
       (frequencies)))

(comment
  ;; Example
  (prn
    (= (let [[_polymer insertion-rules] (parse-input example)]
         (->> (iterate (partial polymerize insertion-rules) "NN")
              (take 6)
              (map #(partition-all 2 1 %))
              (map frequencies)))
       ["NNCB"
        "NCNBCHB"
        "NBCCNBBBCBHCB"
        "NBBBCNCCNBBNBNBBCHBHHBCHB"
        "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"]))

  (let [[_polymer insertion-rules] (parse-input example)]
    (next-generation-pairs insertion-rules "NN"))

  (frequencies "NBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBBNBBNBBNBBNBBNBBNBBNBBNBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCNBBNBBNBBNBBNBBNBNBBNBBNBBNBBNBBCNCCNBBBCCNBCNCCNBBNBBNBBBNBBNBBCCNBCNCCNBBNBNBBCNCCNBBBCCNBCNCCN")
  (let [[polymer insertion-rules] (parse-input example)]
    (->> (partition-all 2 1 polymer)
         (map #(apply str %))
         (map #(apply str (interpose (insertion insertion-rules %) %))))))

(defn expand-pair
  [insertion-rules pair]
  (if (= 1 (count pair))
    {pair 1}
    (->> (polymerize insertion-rules pair)
         (partition 2 1)
         (map #(apply str %))
         (frequencies))))

(t/deftest expand-pair-test
  (t/testing "Expanding a pair results in a frequency map of the next generation's pairs"
    (t/is (= {"NC" 1 "CN" 1} (expand-pair {"NN" "C"} "NN"))))
  (t/testing "Expanding a tail results in a frequency map containing just that tail"
    (t/is (= {"N" 1} (expand-pair {"NN" "C"} "N")))))

(defn polymer-distribution
  [polymer]
  (->> (partition-all 2 1 polymer)
       (map #(apply str %))
       (frequencies)))

(defn unordered-polymerization
  [insertion-rules polymer-distribution]
  (->> (for [[pair count] polymer-distribution
             [child-pair child-count] (expand-pair insertion-rules pair)]
         {child-pair (* count child-count)})
       (apply merge-with +)))

(t/deftest unordered-polymerization-test
  (t/testing "Polymerizing a frequency map of pairs produces the next frequency map of pairs"
    (t/is (= {"NC" 1, "CC" 1, "CN" 1, "NN" 1, "N" 1}
             (unordered-polymerization
               {"NN" "C" "NC" "C" "CN" "N"}
               {"NC" 1, "CN" 1, "N" 1})))

    (t/is (= {"NC" 2, "CC" 2, "CN" 2, "NN" 1, "N" 1}
             (unordered-polymerization
               {"NN" "C" "NC" "C" "CN" "N"}
               {"NC" 1, "CC" 1, "CN" 1, "NN" 1, "N" 1})))))

(comment
  (take 3 (iterate (partial unordered-polymerization {"NN" "C" "NC" "C" "CN" "N"}) (polymer-distribution "NCN"))))

(defn part-1
  []
  (let [[polymer insertion-rules] (parse-input input)
        steps 10]
    (->> (iterate (partial polymerize insertion-rules) polymer)
         (take (inc steps))
         (last)
         (frequencies)
         (map second)
         (sort)
         ((juxt last first))
         (apply -))))

(defn part-1-unordered
  []
  (let [[polymer insertion-rules] (parse-input input)
        steps 10]
    (->> (iterate (partial unordered-polymerization insertion-rules) (polymer-distribution polymer))
         (take (inc steps))
         (last)
         (map #(hash-map (ffirst %) (second %)))
         (apply merge-with +)
         (map second)
         (sort)
         ((juxt last first))
         (apply -))))

(defn part-2
  []
  (let [[polymer insertion-rules] (parse-input input)
        steps 40]
    (->> (iterate (partial unordered-polymerization insertion-rules) (polymer-distribution polymer))
         (take (inc steps))
         (last)
         (map #(hash-map (ffirst %) (second %)))
         (apply merge-with +)
         (map second)
         (sort)
         ((juxt last first))
         (apply -))))
