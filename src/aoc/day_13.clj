(ns aoc.day-13
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def example "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
(def input (util/read-input))

(defn parse-input
  [input]
  (let [[points folds] (str/split input #"\n\n")]
    [(set (->> (str/split-lines points)
               (map #(mapv parse-long (re-seq #"\d+" %)))))
     (->> (str/split-lines folds)
          (map #(let [[_ axis coord] (re-find #"(x|y)=(\d+)" %)]
                  [axis (parse-long coord)])))]))

(defn on-folded-side?
  [[axis coord] p]
  (<= coord (get p (case axis "x" 0 "y" 1))))

(defn reflect
  [[axis coord] p]
  (update p (case axis "x" 0 "y" 1) #(- % (* 2 (- % coord)))))

(defn fold-points
  [points fold]
  (let [{unmoved false moved true} (group-by (partial on-folded-side? fold) points)]
    (set (concat unmoved (map (partial reflect fold) moved)))))

(defn print-points
  [points]
  (let [xs (range (inc (apply max (map first points))))
        ys (range (inc (apply max (map second points))))]
    (doseq [y ys]
      (prn (apply str
                  (for [x xs]
                    (if (points [x y]) "#" ".")))))))

(comment
  (print-points (apply fold-points (update (parse-input example) 1 first)))
  (print-points (first (parse-input example))))

(defn part-1
  []
  (let [[points folds] (parse-input input)]
    (count (fold-points points (first folds)))))

(defn part-2
  []
  (print-points (apply reduce fold-points (parse-input input))))
