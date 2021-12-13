(ns aoc.day-11
  (:require [clojure.string :as str]
            [aoc.util :as util]
            [clojure.set :as set]
            [malli.core :as malli]
            [malli.clj-kondo :as mc]
            [malli.instrument :as mi]
            [clojure.data.priority-map :as pm]))

(def Point2D-10x10 [:vector {:min 2 :max 2} [:int {:min 0 :max 9}]])
(def OctopusSystem [:map-of Point2D-10x10 [:int {:min 0 :max 9}]])
(def TemporaryOctopusSystem [:map-of Point2D-10x10 [:int {:min 0 :max 18}]])

(def input (str/trim (util/read-input)))

(def example-input
  "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map-indexed
         (fn [y row]
           (map-indexed
             (fn [x octopus]
               [[x y] (parse-long (str octopus))])
             row)))
       (apply concat)
       (into {})))

(def neighbors
  (memoize
    (fn [all-pts p]
      (set/intersection
        (set (map #(mapv + % p) (for [dx [-1 0 1]
                                      dy [-1 0 1]]
                                  [dx dy])))
        (disj (set all-pts) p)))))

(defn prn-identity [x] (prn x) x)

(defn flash-tens
  [system]
  (-> (loop [wave 0
             expended {}
             unprocessed system]
        (let [{flashing true charging false} (group-by #(< 9 (second %)) unprocessed)
              flashing (into {} flashing)
              charging (into {} charging)
              _ (assert (malli/validate TemporaryOctopusSystem flashing))
              _ (assert (malli/validate TemporaryOctopusSystem charging))
              charged (->> (mapcat
                             (partial neighbors (map first charging))
                             (map first flashing))
                           (frequencies)
                           (merge-with + charging))
              _ (assert (malli/validate TemporaryOctopusSystem charged))]
          (if (= charging charged)
            (merge expended flashing charged)
            (recur (inc wave) (merge expended flashing) charged))))
      (util/map-vals #(if (> % 9) 0 %))))

(defn flash-priority
  [system]
  (let [system (apply pm/priority-map-by > system)
        _p (pop system)]
    (loop [system system
           flashed {}]
      (if (empty? system)
        flashed
        ()))))

(-> (pm/priority-map-by > :a 1 :b 2)
    (update :a + 3))

(comment
  (flash-tens (util/map-vals (parse-input example-input) (comp inc inc)))
  (malli/validate OctopusSystem (parse-input example-input)))

(defn print-system
  [system]
  (->> (sort-by (juxt second first) (keys system))
       (partition-by second)
       (map #(apply str (map system %)))))

(malli/=> print-system [:=> [:cat OctopusSystem] [:cat :string]])

(defn brute-step
  [system]
  (flash-tens (util/map-vals system inc)))

(defn count-flashes
  [system]
  (count (filter #{0} (vals system))))

(defn part-1
  []
  (->> (iterate brute-step (parse-input input))
       (take 101)
       (map count-flashes)
       (reduce +)))

(defn part-2
  []
  (time (->> (iterate brute-step (parse-input input))
             (keep-indexed #(when (= #{0} (set (vals %2)))
                              %1))
             (first))))

(comment (malli/function-schemas)
         (mi/instrument!))

(comment
  (-> (mc/collect *ns*) (mc/linter-config))
  (mc/emit!)
  ((requiring-resolve 'malli.clj-kondo/emit!)))
