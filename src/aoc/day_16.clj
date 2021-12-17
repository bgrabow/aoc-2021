(ns aoc.day-16
  (:require [aoc.util :as util]
            [instaparse.core :as insta]
            [clojure.test :as t]
            [matcher-combinators.test :refer [match?]]))

(def input (util/read-input))
(def samples
  {:2-subpackets "00111000000000000110111101000101001010010001001000000000"
   :3-subpackets "11101110000000001101010000001100100000100011000001100000"
   :1-literal-packet "110100101111111000101000"})

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
  (let [consumed (->> (partition 5 s)
                      (take-until #(= \0 (first %))))
        remainder (apply str (drop (->> (map count consumed)
                                        (reduce +))
                                   s))]
    [{:packet/literal-value (-> (->> consumed
                                     (mapcat next)
                                     (apply str))
                                (Long/valueOf 2))}
     remainder]))

(declare parse-packet)

(defn parse-subpacket
  [state]
  ;(pprint/pprint state)
  (let [[subpackets remainder] state]
    (when-let [subpacket (parse-packet remainder)]
      (update subpacket 0 #(conj subpackets %)))))

(defn parse-op-type-0
  [s]
  ;(prn "parse-type-0" s)
  (let [n-subpacket-bits (-> (apply str (take 15 s))
                             (Long/valueOf 2))
        remainder (apply str (->> s
                                  (drop 15)
                                  (drop n-subpacket-bits)))
        [subpackets _zero-remainder] (->> (iterate parse-subpacket [[] (take n-subpacket-bits (drop 15 s))])
                                          (take-while some?)
                                          (last))]
    #_(prn {:packet/n-subpacket-bits n-subpacket-bits
            :some-type-0-bits        (apply str (take 10 remainder))
            :size-bits (apply str (take 15 s))
            :s                       (apply str s)
            :remaining-bits (count remainder)
            :zero-remainder zero-remainder})
    [{:packet/n-subpacket-bits n-subpacket-bits
      :packet/subpackets       subpackets}
     remainder]))

(defn parse-op-type-1
  [s]
  (let [n-subpackets (-> (apply str (take 11 s))
                         (Long/valueOf 2))
        [subpackets remainder] (->> (iterate parse-subpacket [[] (apply str (drop 11 s))])
                                    (take (inc n-subpackets))
                                    (last))]
    #_(prn {:packet/n-subpackets n-subpackets
            :size-bits (apply str (take 11 s))})
    [{:packet/n-subpackets n-subpackets
      :packet/subpackets subpackets}
     (apply str remainder)]))

(defn parse-operator
  [s]
  (let [length-type (first s)]
    ;(prn {:length-type length-type})
    (update
      (case length-type
        \0 (parse-op-type-0 (next s))
        \1 (parse-op-type-1 (next s)))
      0
      assoc
      :packet/length-type length-type)))

(defn packet-parser
  [type]
  (case type
    "100" parse-literal
    parse-operator))

(defn parse-header
  [bin-str]
  (let [version (apply str (take 3 bin-str))
        packet-type (apply str (take 3 (drop 3 bin-str)))]
    #_(prn {:packet/version version
            :packet/type    packet-type
            :remaining-bits (count (drop 6 bin-str))})
    [{:packet/version version
      :packet/type    packet-type}
     (apply str (drop 6 bin-str))]))

(defn parse-contents
  [[header remainder]]
  (-> ((packet-parser (:packet/type header)) remainder)
      (update 0 merge header)))

(defn parse-packet
  [bin-str]
  ;(prn "parse-packet" (apply str bin-str))
  (when (seq bin-str)
    (parse-contents (parse-header bin-str))))

(defn split-into-sizes
  [sizes coll]
  (if (seq sizes)
    (cons (take (first sizes) coll)
          (split-into-sizes (rest sizes) (drop (first sizes) coll)))
    (list coll)))

(defn versions
  [packet-tree]
  (when (seq packet-tree)
    (cons (:packet/version packet-tree)
          (mapcat versions (:packet/subpackets packet-tree)))))

(defn value
  [packet-tree]
  (case (Long/valueOf (:packet/type packet-tree) 2)
    0 (reduce + (map value (:packet/subpackets packet-tree)))
    1 (reduce * (map value (:packet/subpackets packet-tree)))
    2 (reduce min (map value (:packet/subpackets packet-tree)))
    3 (reduce max (map value (:packet/subpackets packet-tree)))
    4 (:packet/literal-value packet-tree)
    5 (if (apply > (map value (:packet/subpackets packet-tree))) 1 0)
    6 (if (apply < (map value (:packet/subpackets packet-tree))) 1 0)
    7 (if (apply = (map value (:packet/subpackets packet-tree))) 1 0)))

(defn part-1
  []
  (->> (versions (first (parse-packet (parse-input input))))
       (map #(Long/valueOf % 2))
       (reduce +)))

(defn part-2
  []
  (value (first (parse-packet (parse-input input)))))

(comment
  (parse-packet (:1-literal-packet samples))
  (parse-packet (:2-subpackets samples))
  (versions (first (parse-packet (:3-subpackets samples))))
  (parse-packet (parse-input input))
  (parse-packet "110100101111111000101000")
  (map #(apply str %)
       (split-into-sizes
         [3 3 1 15 3 3 5 3 3 5 5]
         "00111000000000000110111101000101001010010001001000000000")))

(def PacketGrammar
  "S = packet unparsed-tail
  digit = '0' | '1'
  packet = version contents
  version = #'[01]{3}'
  literal-type = '100'
  operator-type = '000' | '001' | '010' | '011' | '101' | '110' | '111'
  contents = (literal-type literal-contents) | (operator-type)
  literal-contents = {non-terminal-bits}+ terminal-bits
  operator-contents = ('0' bit-length-digits packet+) | ('1' n-packets-digits packet+)
  unparsed-operator = #'[01]+'
  unparsed-tail = #'[01]+'
  n-packets-digits = #'[01]{11}'
  bit-length-digits = #'[01]{15}'
  non-terminal-bits = '1' value-bits
  terminal-bits = '0' value-bits
  ;value-bits = #'[01]{4}'
  value-bits = digit{4}
  tail = '0'+")

(def PacketGrammarABNF
  "S = packet unparsed-tail
  digit = '0' / '1'
  packet = version contents
  version = #'[01]{3}'
  literal-type = '100'
  operator-type = '000' / '001' / '010' / '011' / '101' / '110' / '111'
  contents = (literal-type literal-contents) / (operator-type)
  literal-contents = 1*non-terminal-bits terminal-bits
  operator-contents = ('0' bit-length-digits 1*packet) / ('1' n-packets-digits 1*packet)
  unparsed-operator = 1*BIT
  unparsed-tail = 1*BIT
  n-packets-digits = 11*11BIT
  bit-length-digits = 15*15BIT
  non-terminal-bits = '1' value-bits
  terminal-bits = '0' value-bits
  value-bits = 4*4BIT
  tail = 1*'0'")

(comment
  (insta/parse (insta/parser PacketGrammarABNF :input-format :abnf)
               (:1-literal-packet samples) :partial true)
  (insta/parse (insta/parser PacketGrammarABNF :input-format :abnf)
               (:2-subpackets samples) :partial true)
  (insta/parse (insta/parser PacketGrammar)
               (:3-subpackets samples) :partial true)
  (insta/parse (insta/parser PacketGrammar)
               (parse-input input)))

(defn packet-chunk
  [packet-type]
  (case packet-type
    "100" :chunk/literal-value))

(defn consume-literal-value
  [state]
  (let [s (:remaining state)
        consumed (->> (partition 5 s)
                      (take-until #(= \0 (first %))))
        remainder (apply str (drop (->> (map count consumed)
                                        (reduce +))
                                   s))
        literal-value (-> (->> consumed
                               (mapcat next)
                               (apply str))
                          (Long/valueOf 2))]
    {:tree (assoc-in
             (:tree state)
             (concat (:cursor state) [:packet/literal-value])
             literal-value)
     :context (pop (:context state))
     :cursor (:cursor state)
     :remaining remainder}))

(defn consume-packet-header
  [state]
  (let [s (:remaining state)
        packet-type (subs s 3 6)]
    {:tree      (assoc (:tree state)
                  :packet/version (subs s 0 3)
                  :packet/type packet-type)
     :context   (conj (pop (vec (:context state))) (packet-chunk packet-type))
     :cursor    []
     :remaining (subs s 6)}))

(defn consume-chunk
  [state]
  (case (peek (:context state))
    :chunk/literal-value (consume-literal-value state)
    :chunk/packet-header (consume-packet-header state)
    state))

(t/deftest consume-chunk-test
  (t/testing "Consuming a chunk with no prior context starts a new packet"
    (t/is (match? {:tree {:packet/version "110"
                          :packet/type "100"}
                   :remaining "101111111000101000"}
             (consume-chunk {:remaining "110100101111111000101000"
                             :context [:chunk/packet-header]}))))
  (t/testing "When a literal packet's header is read, the next chunk consumed is the literal value"
    (t/is (match? {:tree {:packet/version "110"
                          :packet/type "100"
                          :packet/literal-value 2021}
                   :remaining "000"}
                  (last (take 3 (iterate consume-chunk {:remaining "110100101111111000101000"
                                                        :context [:chunk/packet-header]}))))))
  (t/testing "Continuing to consume chunks past the end of a chunk produces a fixed point"
    (t/is (match? {:tree {:packet/version "110"
                          :packet/type "100"
                          :packet/literal-value 2021}
                   :remaining "000"}
                  (last (take 4 (iterate consume-chunk {:remaining "110100101111111000101000"
                                                        :context [:chunk/packet-header]})))))))

(t/run-test consume-chunk-test)
