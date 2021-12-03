(ns aoc.day-02
  (:require [clojure.string :as str]
            [aoc.util :as util]
            [clojure.test :as t]))

(def unit-vector
  {"down" [0 1]
   "up" [0 -1]
   "forward" [1 0]})

(defn step
  [s]
  (let [[direction distance] (str/split s #" ")]
    (mapv * (unit-vector direction) (repeat (parse-long distance)))))

(def input
  (->> (util/read-input)
       (str/split-lines)))

(def example-input (str/split-lines "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"))

(defn follow-course
  [segments]
  (->> segments
       (map step)
       (reduce #(mapv + %1 %2))))

(comment
  (follow-course example-input))

(defn part-1
  []
  (reduce * (follow-course input)))

(t/deftest example
  (t/is (= 150 (reduce * (follow-course example-input))))
  (t/is (= :fail (reduce * (follow-course input)))))

(defn up
  [[position depth aim] distance]
  [position depth (- aim distance)])

(defn down
  [[position depth aim] distance]
  [position depth (+ aim distance)])

(defn forward
  [[position depth aim] distance]
  [(+ position distance) (+ depth (* aim distance)) aim])

(defn move
  [submarine [direction distance]]
  ((resolve (symbol direction)) submarine (parse-long distance)))

(defn follow-aimed-course
  [segments]
  (reduce move [0 0 0] segments))

(defn part-2
  []
  (->> input
       (map #(str/split % #" "))
       (follow-aimed-course)
       (take 2)
       (reduce *)))
