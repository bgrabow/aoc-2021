(ns aoc.day-22
  (:require [aoc.util :as util]
            [clojure.string :as str]))
            ;[clojure.set :as set]))

(def input (util/read-input))
(def init-area {:x [-50 50] :y [-50 50] :z [-50 50]})

(defn parse-reboot-step
  [s]
  (let [state (re-find #"off|on" s)]
    (into {:state state}
      (for [[axis min max] (partition 3 (re-seq #"x|y|z|-?\d+" s))]
        [(keyword axis) [(parse-long min) (parse-long max)]]))))

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map parse-reboot-step)))

(defn ranges-overlap?
  [[a-min a-max] [b-min b-max]]
  (or (<= a-min b-min a-max)
      (<= a-min b-max a-max)
      (<= b-min a-min b-max)
      (<= b-min a-min b-max)))

(defn cubes-intersect?
  [a b]
  (->> (map (juxt a b) [:x :y :z])
       (every? #(apply ranges-overlap? %))))

(defn cubes
  [cuboid]
  (for [x (apply range (update (:x cuboid) 1 inc))
        y (apply range (update (:y cuboid) 1 inc))
        z (apply range (update (:z cuboid) 1 inc))]
    [x y z]))

(defn in-cuboid?
  [cuboid [x y z]]
  (and (apply <= (interpose x (:x cuboid)))
       (apply <= (interpose y (:y cuboid)))
       (apply <= (interpose z (:z cuboid)))))

(defn process-step
  [cubes-on step]
  (case (:state step)
    "off" (reduce disj cubes-on (cubes step))
    "on" (reduce conj cubes-on (cubes step))))

(defn part-1
  []
  (->> (parse-input input)
       (filter #(cubes-intersect? init-area %))
       (reduce process-step #{})
       (count)))

(comment
  (cubes-intersect?
    {:state "on", :x [31699 37219], :y [-36521 -23135], :z [61018 77965]}
    {:state "on", :x [-13575 31700], :y [-33410 -19901], :z [59130 95383]})

  (apply ranges-overlap? [[31699 37219] [-13575 31700]])
  (apply ranges-overlap? [[-36521 -23135] [-33410 -19901]])
  (apply ranges-overlap? [[61018 77965] [59130 95383]]))
