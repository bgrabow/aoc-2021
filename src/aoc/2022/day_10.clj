(ns aoc.2022.day-10
  (:require [clojure.string :as str]))

(def input (slurp "resources/2022/input10.txt"))
(def example-1 "noop\naddx 3\naddx -5")
(def example-2 "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n")

(defn parse-input
  [s]
  (->> (str/split-lines s)
       (map #(-> (zipmap [:op :n] (str/split % #" "))
                 (update :n (fnil parse-long "0"))
                 (update :op keyword)))))

(defn step
  [states {:keys [op n]}]
  (let [{:keys [x cycle]} (last states)]
    (case op
      :noop [{:x x :cycle (inc cycle)}]
      :addx [{:x x :cycle (inc cycle)}
             {:x (+ x n) :cycle (+ 2 cycle)}])))

(defn signal-strength
  [{:keys [x cycle]}]
  (* x cycle))

(defn interesting-cycle
  [{:keys [cycle]}]
  (= 0 (rem (- cycle 20) 40)))

(defn solve-1
  [s]
  (->> (reductions step [{:x 1 :cycle 1}] (parse-input s))
       (apply concat)
       (filter interesting-cycle)
       (map #(assoc % :signal-strength (signal-strength %)))
       (map :signal-strength)
       (reduce +)))

(comment
  (solve-1 input))
