(ns aoc.util
  (:require [clojure.string :as str]))

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
  (apply map vector colls))

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
