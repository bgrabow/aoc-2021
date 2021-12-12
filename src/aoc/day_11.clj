(ns aoc.day-11
  (:require [clojure.string :as str]
            [aoc.util :as util]
            [clojure.set :as set]
            [malli.core :as malli]
            [malli.generator :as mg]
            [malli.clj-kondo :as mc]
            [malli.instrument :as mi]))

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

(defn step
  [system]
  (let [increments (zipmap (map first system) (repeat 1))]
    (->> system
         (sort-by (comp - second))
         (reduce
           (fn [increments [p octopus]]
             (if (< 9 (+ octopus (get increments p)))
               (reduce #(update %1 %2 inc) increments (neighbors (keys increments) p))
               increments))
           increments)
         (merge-with
           (fn [x y]
             (if (<= (+ x y) 9)
               (+ x y) 0))
           (into {} system)))))

(def will-flash?
  (memoize
    (fn [system p]
      (cond
        (< 9 (get system p)) true
        (>= 9 (+ (get system p) 8)) false
        :else (< 9 (+ (get system p)
                      (count
                        (keep
                          (partial will-flash? system)
                          (neighbors (keys system) p)))))))))

(def increment
  (memoize
    (fn [system p]
      (+ (get system p)
         1
         (count
           (keep #(> % 9)
                 (map (partial increment system)
                      (neighbors system p))))))))

(defn map-vals
  [m f]
  (reduce
    (fn [m [k v]]
      (assoc m k (f v)))
    {}
    m))

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
            (do
              ;(prn wave)
              (merge expended flashing charged))
            (recur (inc wave) (merge expended flashing) charged))))
      (map-vals #(if (> % 9) 0 %))))

(comment
  (flash-tens (map-vals (parse-input example-input) (comp inc inc)))
  (malli/validate OctopusSystem (parse-input example-input)))

(defn print-system
  [system]
  (->> (sort-by (juxt second first) (keys system))
       (partition-by second)
       (map #(apply str (map system %)))))

(malli/=> print-system [:=> [:cat OctopusSystem] [:cat :string]])

(defn brute-step
  [system]
  (flash-tens (map-vals system inc)))

(comment (mg/generate OctopusSystem)
         (map print-system (take 10 (iterate brute-step (parse-input example-input)))))

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

(comment (print-system (step (step (step (parse-input example-input))))))
(comment (malli/function-schemas)
         (mi/instrument!))

(comment
  (-> (mc/collect *ns*) (mc/linter-config))
  (mc/emit!)
  ((requiring-resolve 'malli.clj-kondo/emit!)))
