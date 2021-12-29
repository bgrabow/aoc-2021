(ns aoc.day-17
  (:require [aoc.util :as util]))

(def input (util/read-input))
(def sample "target area: x=20..30, y=-10..-5")

(defn parse-input
  [input]
  (zipmap
    [:x-min :x-max :y-min :y-max]
    (map parse-long (re-seq #"-?\d+" input))))

(comment
  (parse-input input))

(defn in-target?
  [target [x y]]
  (and (<= (:x-min target) x (:x-max target))
       (<= (:y-min target) y (:y-max target))))

(defn past-target?
  [target [x y]]
  (or (< (:x-max target) x)
      (< y (:y-min target))))

(comment
  (past-target? (parse-input input) [158 -146]))

(defn step
  [[p v]]
  [(mapv + p v)
   (-> v
       (update 0 #(max 0 (dec %)))
       (update 1 dec))])

(comment
  (take 40 (iterate step [[0 0] [5 10]])))

(defn hits-target?
  [target [p v]]
  (cond
    (in-target? target p) true
    (past-target? target p) false
    :else (hits-target? target (step [p v]))))

(defn done?
  [target [p _v]]
  (or (in-target? target p)
      (past-target? target p)))

(defn trajectory
  [target [p v]]
  (util/take-until (partial done? target) (iterate step [p v])))

(defn print-trajectory
  [target trajectory]
  (let [[xmin xmax] ((juxt first last) (sort (concat (map ffirst trajectory)
                                                     ((juxt :x-min :x-max) target))))
        [ymin ymax] ((juxt first last) (sort (concat (map (comp second first) trajectory)
                                                     ((juxt :y-min :y-max) target))))
        traj-points (set (map first trajectory))]
    (doseq [y (reverse (range (- ymin 2) (+ ymax 3)))]
      (println
        (apply str
          (for [x (range (- xmin 2) (+ xmax 3))]
            (cond
              (= [0 0] [x y]) \S
              (contains? traj-points [x y]) \#
              (in-target? target [x y]) \T
              :else \.)))))))

(defn high-point
  [trajectory]
  (last (sort (map (comp second first) trajectory))))

(comment
  (hits-target? (parse-input input) [[92 -136] [10 -10]])
  (hits-target? (parse-input sample) [[0 0] [7 2]])
  (print-trajectory (parse-input sample)
                    (trajectory (parse-input sample) [[0 0] [7 2]]))
  (hits-target? (parse-input sample) [[0 0] [6 3]])
  (hits-target? (parse-input sample) [[0 0] [9 0]])
  (hits-target? (parse-input sample) [[0 0] [17 -4]])
  (print-trajectory (parse-input sample)
                    (trajectory (parse-input sample) [[0 0] [17 -4]]))
  (print-trajectory (parse-input sample)
                    (trajectory (parse-input sample) [[0 0] [7 9]]))
  (print-trajectory (parse-input input)
                    (trajectory (parse-input input) [[0 0] [15 240]])))

(defn part-1
  []
  (let [vx 15
        target (parse-input input)]
    (->> (for [vy (range 0 500)
               :when (hits-target? target [[0 0] [vx vy]])]
           [vx vy])
         (sort-by second)
         (last)
         (#(trajectory target [[0 0] %]))
         (high-point))))

(defn part-2
  []
  (let [target (parse-input input)]
    (count (for [vx (range 0 (inc (:x-max target)))
                 vy (range (:y-min target) (long (Math/abs ^Double (:y-min target))))
                 :when (hits-target? target [[0 0] [vx vy]])]
             [vx vy]))))
