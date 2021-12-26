(ns aoc.day-25
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def input (util/read-input))

(def samples {:realistic "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>"
              :realistic-stepped "....>.>v.>\nv.v>.>v.v.\n>v>>..>v..\n>>v>v>.>.v\n.>v.v...v.\nv>>.>vvv..\n..v...>>..\nvv...>>vv.\n>.v.v..v.v"
              :realistic-2-step ">.v.v>>..v\nv.v.>>vv..\n>v>.>.>.v.\n>>v>v.>v>.\n.>..v....v\n.>v>>.v.v.\nv....v>v>.\n.vv..>>v..\nv>.....vv."
              :e-s-precedence "..........\n.>v....v..\n.......>..\n.........."
              :donut "...>...\n.......\n......>\nv.....>\n......>\n.......\n..vvv.."
              :donut-1-step "..vv>..\n.......\n>......\nv.....>\n>......\n.......\n....v..\n"})

(defn parse-cucumbers
  [input]
  (util/parse-2d-grid
    (fn [p c]
      (when (not= \. c)
        [p c]))
    input))

(defn parse-bounds
  [input]
  ((juxt (comp count first) count) (str/split-lines input)))

(comment
  (parse-bounds (:e-s-precedence samples)))

(defn parse-input
  [input]
  ((juxt parse-bounds parse-cucumbers) input))

(defn south-neighbor
  [bounds p]
  (update p 1 #(rem (inc %) (second bounds))))

(defn step-south
  [[bounds cucumbers]]
  (let [eastbound (filter (fn [[_ c]] (= \> c)) cucumbers)
        southbound (filter (fn [[_ c]] (= \v c)) cucumbers)]
    [bounds (->> (map first southbound)
                 (map #(if (get cucumbers (south-neighbor bounds %))
                         [% \v]
                         [(south-neighbor bounds %) \v]))
                 (concat eastbound)
                 (into {}))]))

(defn east-neighbor
  [bounds p]
  (update p 0 #(rem (inc %) (first bounds))))

(defn step-east
  [[bounds cucumbers]]
  (let [eastbound (filter (fn [[_ c]] (= \> c)) cucumbers)
        southbound (filter (fn [[_ c]] (= \v c)) cucumbers)]
    [bounds (->> (map first eastbound)
                 (map #(if (get cucumbers (east-neighbor bounds %))
                         [% \>]
                         [(east-neighbor bounds %) \>]))
                 (concat southbound)
                 (into {}))]))

(comment
  (step-east (parse-input (:e-s-precedence samples))))

(defn step
  [system]
  (step-south (step-east system)))

(comment
  (step (step (parse-input (:e-s-precedence samples))))
  (map #(update % 1 sort) (take 3 (iterate step (parse-input (:donut samples)))))
  (update (parse-input (:donut-1-step samples)) 1 sort)
  (= (parse-input (:realistic-stepped samples))
     (step (parse-input (:realistic samples)))))

(defn part-1
  []
  (count (util/iterate-until-fixed step (parse-input input))))
