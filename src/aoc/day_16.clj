(ns aoc.day-16
  (:require [aoc.util :as util]))

(def input (util/read-input))

(def hex->bin
  {"0" "0000"
   "1" "0001"
   "2" "0010"
   "3" "0011"
   "4" "0100"
   "5" "0101"
   "6" "0110"
   "7" "0111"
   "8" "1000"
   "9" "1001"
   "A" "1010"
   "B" "1011"
   "C" "1100"
   "D" "1101"
   "E" "1110"
   "F" "1111"})

(defn parse-input
  [input]
  (apply str (mapcat (comp hex->bin str) input)))

(defn take-until
  [f coll]
  (let [[left right] (split-with (complement f) coll)]
    (concat left (take 1 right))))

(defn parse-literal
  [s]
  {:packet/literal-value (-> (->> (partition 5 s)
                                  (take-until #(= \0 (first %)))
                                  (mapcat next)
                                  (apply str))
                             (Long/valueOf 2))})

(defn parse-operator
  [_s])

(defn packet-parser
  [type]
  (case type
    "100" parse-literal
    parse-operator))

(defn parse-packet
  [bin-str]
  (let [version (apply str (take 3 bin-str))
        packet-type (apply str (take 3 (drop 3 bin-str)))]
    (merge {:packet/version version
            :packet/type    packet-type}
           ((packet-parser packet-type) (drop 6 bin-str)))))

(comment
  (parse-packet (parse-input input))
  (parse-packet "110100101111111000101000"))
