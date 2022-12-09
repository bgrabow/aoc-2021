(ns aoc.2022.day-09
  (:require [clojure.string :as str]))

(def input (slurp "resources/2022/input09.txt"))

(def direction
  {"U" [0 1]
   "D" [0 -1]
   "L" [-1 0]
   "R" [1 0]})

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (map #(-> (zipmap [:dir :dist] (re-seq #"\w+" %))
                 (update :dist parse-long)
                 (update :dir direction)))))

(defn too-far?
  [v]
  (not (every? #(<= % 1) (mapv #(Math/abs ^long %) v))))

(defn catch-up
  [h t]
  (let [v (mapv - h t)]
    (if (too-far? v)
      (mapv + t (mapv #(min (max % -1) 1) v))
      t)))

(defn move-1
  [{:keys [h t]} dir]
  (let [h (mapv + h dir)]
    {:h h
     :t (catch-up h t)}))

(defn move-n
  [state {:keys [dir dist]}]
  (reduce move-1 state (repeat dist dir)))

(defn expand-steps
  [steps]
  (mapcat
    (fn [{:keys [dir dist]}]
      (repeat dist dir))
    steps))

(defn state->str
  [range-x range-y start {:keys [h t]}]
  (str (str/join \newline
                 (for [y (reverse range-y)]
                   (apply str
                          (for [x range-x]
                            (cond
                              (= [x y] h) "H"
                              (= [x y] t) "T"
                              (= [x y] start) "s"
                              :else ".")))))
       \newline))

(move-1 {:h [4 4], :t [4 3]} [-1 0])

(def example-1 "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(defn solve-1
  [s]
  (->> (reductions move-1 {:h [0 0] :t [0 0]} (expand-steps (parse-input s)))
       (map :t)
       (set)
       (count)))

(comment
  (solve-1 example-1)
  (solve-1 input))
