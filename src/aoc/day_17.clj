(ns aoc.day-17
  (:require [clojure.string :as str]
            [aoc.util :as util]
            [clojure.test :as t]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combinatorics]))

(def samples "[1,2]\n[[1,2],3]\n[9,[8,7]]\n[[1,9],[8,5]]\n[[[[1,2],[3,4]],[[5,6],[7,8]]],9]\n[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]\n[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")
(def input (util/read-input))

(defn num-str->num
  [s]
  (or (parse-long s) s))

;; RULES
; Nested inside 4 pairs => explode
; Explode:
; * Left value of pair added to first regular number left of pair
; * Right value of pair added to first regular number right of pair
; * If no such neighboring number, result is 0.

(defn parse-input
  [input]
  ((comp
     (partial map num-str->num)
     (partial re-seq #"\[|\]|\d+")) input))

(map parse-input (str/split-lines samples))

(defn depth
  [tree]
  (if (number? tree)
    0
    (apply max (map (comp inc depth) tree))))

(defn explode-into
  [side x]
  (let [[inner outer] (split-with string? side)]
    (if (number? (first outer))
      ;(do)
        ;(prn "explode" outer inner)
        ;(prn "explade" (reverse (into (rest outer) (cons (+ x (first outer)) inner))))
      (into (cons (+ x (first outer)) (rest outer)) (reverse inner))
      side)))

(comment
  (explode-into '("[" "[" "[" "[" "[") 9)
  (explode-into '("[" 4 "[" 5 "[" 6 "[" 7 "[") 9))

(defn explode
  [tree]
  (for [a tree :when (vector? a)
        b a :when (vector? b)
        c b :when (vector? c)
        d c :when (vector? d)]
    [a b c d])
  (loop [left '()
         right (apply list tree)
         depth 0]
    ;(prn "loop" [depth (reverse left) right])
    (if (seq right)
      (if (and (number? (first right))
               (number? (first left))
               (< 4 depth))
        (reverse (into (explode-into (nnext left) (first left))
                       (cons 0 (explode-into (nnext right) (first right)))))

        #_{:left left :right right :depth depth}
        (case (first right)
          "[" (recur (conj left "[") (rest right) (inc depth))
          "]" (recur (conj left "]") (rest right) (dec depth))
          (recur (conj left (first right)) (rest right) depth)))
      (reverse left))))

(t/deftest explode-test
  (t/is (= (parse-input "[[[[0,9],2],3],4]")
           (explode
             (parse-input "[[[[[9,8],1],2],3],4]"))))
  (t/is (= (parse-input "[7,[6,[5,[7,0]]]]")
           (explode
             (parse-input "[7,[6,[5,[4,[3,2]]]]]"))))
  (t/is (= (parse-input "[[6,[5,[7,0]]],3]")
           (explode
             (parse-input "[[6,[5,[4,[3,2]]]],1]"))))
  (t/is (= (parse-input "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
           (explode
             (parse-input "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))))
  (t/is (= (parse-input "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
           (explode
             (explode
               (parse-input "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))))))

(defn split
  [s]
  (let [[left right] (split-with (complement #(and (number? %) (<= 10 %))) s)]
    (apply list (concat left (when-let [double-digit (first right)]
                               (list*
                                 "["
                                 (quot double-digit 2)
                                 (Math/round ^double (/ double-digit 2))
                                 "]"
                                 (rest right)))))))

(t/deftest split-test
  (t/is (= (parse-input "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
           (split
             (parse-input "[[[[0,7],4],[15,[0,13]]],[1,1]]"))))
  (t/is (= (parse-input "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
           (split
             (split
               (parse-input "[[[[0,7],4],[15,[0,13]]],[1,1]]"))))))

(defn iterate-until-fixed
  [f x]
  (ffirst (filter #(apply = %) (partition 2 1 (iterate f x)))))

(comment
  (iterate-until-fixed #(quot % 2) 9))

(defn snailfish-reduce
  [x]
  (iterate-until-fixed
    (comp split (partial iterate-until-fixed explode))
    x))

(comment
  (snailfish-reduce (parse-input "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
  (snailfish-reduce (parse-input "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))

(t/deftest reduce-test
  (t/is (= (parse-input "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
           (snailfish-reduce (parse-input "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")))))

(defn snailfish-add
  [x y]
  (snailfish-reduce (apply list (concat ["["] (snailfish-reduce x) (snailfish-reduce y) ["]"]))))

(defn snailfish-sum
  [xs]
  (reduce snailfish-add xs))

(t/deftest sum-test
  (t/is (= (apply str (parse-input "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
           (apply str (snailfish-sum (map parse-input (str/split-lines "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]"))))))
  (t/is (= (apply str (parse-input "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
           (apply str (snailfish-sum (map parse-input (str/split-lines "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")))))))

(defn magnitude
  [tree]
  (if (number? tree)
    tree
    (+ (* 3 (magnitude (first tree)))
       (* 2 (magnitude (second tree))))))

(defn str->tree
  [s]
  (edn/read-string (apply str (interpose "," s))))

(defn part-1
  []
  (magnitude
    (str->tree
      (snailfish-sum
        (map parse-input
             (str/split-lines input))))))

(defn part-2
  []
  (apply max
         (map (comp magnitude str->tree snailfish-sum)
              (combinatorics/permuted-combinations
                (map parse-input
                     (str/split-lines
                       input))
                2))))

(comment
  (reduce snailfish-add
          (map parse-input (str/split-lines samples)))

  (map snailfish-reduce (map parse-input (str/split-lines samples)))

  (map explode (map parse-input (str/split-lines samples))))
