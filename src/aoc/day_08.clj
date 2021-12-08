(ns aoc.day-08
  (:require [aoc.util :as util]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.set :as set]))

(def example-input
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
 edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
 fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
 fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
 aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
 fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
 dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
 bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
 egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
 gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(def input (util/read-input))

(defn parse-line
  [line]
  (let [[digits outputs] (map #(re-seq #"\w+" %) (str/split line #" \| "))]
    {:digits digits
     :outputs outputs}))

(defn parse-input
  [input]
  (->> (str/split-lines (str/trim input))
       (map str/trim)
       (map parse-line)))

(map char (keys (frequencies (map int example-input))))

(def segments
  "
 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc")

(def digit-segments
  {0 (set "deagbc")
   1 (set "ab")
   2 (set "dafgc")
   3 (set "dafbc")
   4 (set "eafb")
   5 (set "defbc")
   6 (set "defgbc")
   7 (set "dab")
   8 (set "deafgbc")
   9 (set "deafbc")})

(def segments->digit
  (set/map-invert digit-segments))

(defn part-1
  []
  (-> (->> (parse-input input)
           (mapcat :outputs)
           (group-by count))
      (select-keys (map (comp count second) (select-keys digit-segments [1 4 7 8])))
      (vals)
      (->> (apply concat)
           (count))))

(comment
  (* 1 2 3 4 5 6 7) ; => 5040 This is the number of permutations of the translation map from
  ; scrambled signal patterns to ordered signal patterns. To do an exhaustive search of all the permutations
  ; to see which permutation produces a valid mapping to arabic numeral patterns is O(5040*N*M) where N
  ; is the number of scrambled data sets we have and M is the size of the effort to check if the translation
  ; map produces valid arabic numeral patterns. (That check should be O(~1) since we can translate the set of
  ; scrambled patterns and perform an O(1) set equality check to the set of canonical patterns.
  ,)

(defn translation-permutations
  [as]
  (map #(zipmap as %) (combinatorics/permutations as)))

(translation-permutations "abcdefg")

(defn translate
  [decoder s]
  (apply str (map decoder s)))

(defn valid-decoder?
  [decoder input expected-output]
  #_(prn {:decoder         decoder
          :input           input
          :expected-output expected-output
          :actual-output   (set (map #(set (translate decoder %)) input))})
  (= expected-output (set (map #(set (translate decoder %)) input))))

(defn combine-digits
  [xs]
  (parse-long (apply str xs)))

(defn decoded-output
  [{:keys [outputs digits]}]
  (let [decoder (->> (translation-permutations "abcdefg")
                     (filter #(valid-decoder? % digits (set (vals digit-segments))))
                     (first))]
    (->> outputs
         (map (comp segments->digit set (partial translate decoder)))
         (combine-digits))))

(defn part-2
  []
  (->> (parse-input input)
       (map decoded-output)
       (reduce +)))
