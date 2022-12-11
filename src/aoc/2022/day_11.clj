(ns aoc.2022.day-11
  (:require [clojure.string :as str]))

(def input (slurp "resources/2022/input11.txt"))
(def example-1 "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")

(defn find-int
  [s]
  (parse-long (re-find #"\d+" s)))

(defn operation
  [x y z]
  (let [oper (resolve (symbol x))
        op1 (or (parse-long y)
                (symbol y))
        op2 (or (parse-long z)
                op1)]
    (eval `(fn [~op1]
             (~oper ~op1 ~op2)))))

(defn parse-monkey
  [s]
  (-> (zipmap [:monkey :items :operation :test :true-dest :false-dest] (str/split-lines s))
      (update :monkey find-int)
      (update :items #(map parse-long (re-seq #"\d+" %)))
      (update :operation #(let [[_ _ _ op1 oper op2] (re-seq #"\S+" %)]
                            (operation oper op1 op2)))
      (update :test find-int)
      (update :true-dest find-int)
      (update :false-dest find-int)))

(defn index-by
  [f coll]
  (zipmap (map f coll) coll))

(defn parse-input
  [s]
  (index-by :monkey (map parse-monkey (str/split s #"\n\n"))))

(defn throw-item
  [state monkey]
  (let [item (first (:items (get state monkey)))
        worry-level (quot ((:operation (get state monkey)) item) 3)
        test (zero? (rem worry-level (:test (get state monkey))))
        dest (if test
               (:true-dest (get state monkey))
               (:false-dest (get state monkey)))]
    (-> state
        (update-in [monkey :items] next)
        (update-in [dest :items] #(conj (vec %) worry-level)))))

(defn throw-item-2
  [state monkey]
  (let [item (first (:items (get state monkey)))
        lcm (apply * (map :test (vals state)))
        worry-level (rem ((:operation (get state monkey)) item) lcm)
        test (zero? (rem worry-level (:test (get state monkey))))
        dest (if test
               (:true-dest (get state monkey))
               (:false-dest (get state monkey)))]
    (-> state
        (update-in [monkey :items] next)
        (update-in [dest :items] #(conj (vec %) worry-level)))))

(defn throw-all
  [state monkey]
  (->> (iterate #(throw-item % monkey) state)
       (drop-while #(seq (:items (get % monkey))))
       (first)))

(defn throw-all-2
  [state monkey]
  (->> (iterate #(throw-item-2 % monkey) state)
       (drop-while #(seq (:items (get % monkey))))
       (first)))

(defn round
  [state]
  (reductions throw-all state (sort (keys state))))

(defn round-2
  [state]
  (reductions throw-all-2 state (sort (keys state))))

(defn round->inspected-items
  [round]
  (zipmap
    (keys (first round))
    (for [monkey (keys (first round))]
      (count (get-in (nth round monkey) [monkey :items])))))

(defn monkey-business
  [inspections]
  (->> (vals inspections)
       (sort-by -)
       (take 2)
       (apply *)))

(defn solve-1
  [s]
  (->> (iterate (comp round last) (round (parse-input s)))
       (take 20)
       (map round->inspected-items)
       (apply merge-with +)
       (monkey-business)))

(comment
  (solve-1 input))

(defn solve-2
  [s]
  (->> (iterate (comp round-2 last) (round-2 (parse-input s)))
       (take 10000)
       (map round->inspected-items)
       (apply merge-with +)
       (monkey-business)))

(comment
  (solve-2 input))
