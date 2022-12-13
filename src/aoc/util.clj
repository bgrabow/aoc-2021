(ns aoc.util
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as pm]))

(defn read-input*
  [ns]
  (slurp (format "resources/input/%s.txt"
                 (-> (ns-name ns)
                     str
                     (str/split #"\.")
                     (last)
                     (str/replace #"-" "_")))))

(defmacro read-input
  []
  `(let [ns# *ns*]
     (read-input* ns#)))

(defn map-vals
  [m f]
  (reduce
    (fn [m [k v]]
      (assoc m k (f v)))
    {}
    m))

(defn transpose
  [colls]
  (vec (apply map vector colls)))

(defn iterate-until-fixed
  "Iterates f on x in the style of clojure.core/iterate. Returns the sequence of results up until
  the sequence reaches a fixed point.

  e.g.
  (defn ensmallen
    [x]
    (long (Math/floor (Math/sqrt x))))

  (iterate-until-fixed ensmallen 99) => (99, 9, 3, 1)"
  [f x]
  (cons x (map second (take-while #(apply not= %) (partition 2 1 (iterate f x))))))

(defn parse-2d-grid
  [f s]
  (->> (str/split-lines s)
       (map-indexed
         (fn [y row]
           (keep-indexed
             (fn [x c]
               (f [x y] c))
             row)))
       (apply concat)
       (into {})))

(defn take-until
  [f coll]
  (let [[l r] (split-with (complement f) coll)]
    (concat l (take 1 r))))

(defn step-dijkstra
  [neighbors-fn step-cost-fn state]
  (let [[pos cost] (peek (:frontier state))]
    {:frontier (reduce
                 conj
                 (pop (:frontier state))
                 (->> (neighbors-fn pos)
                      (remove (:visited state))
                      (remove (:frontier state))
                      (map (juxt identity #(+ cost (step-cost-fn pos %))))))
     :visited  (conj (:visited state) (peek (:frontier state)))}))

(defn dijkstra
  [neighbors-fn step-cost-fn origin destination]
  (->> (iterate
         (partial step-dijkstra neighbors-fn step-cost-fn)
         {:frontier (pm/priority-map origin 0)
          :visited  {}})
       (take-while #(seq (:frontier %)))
       (filter #((:visited %) destination))
       (first)))

(defn dijkstra-2
  [neighbors-fn step-cost-fn origin destination-fn]
  (->> (iterate
         (partial step-dijkstra neighbors-fn step-cost-fn)
         {:frontier (pm/priority-map origin 0)
          :visited  {}})
       (take-while #(seq (:frontier %)))
       (filter #(some (comp destination-fn first) (:visited %)))
       (first)))
