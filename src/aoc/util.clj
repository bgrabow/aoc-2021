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
