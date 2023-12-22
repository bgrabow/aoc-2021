(ns aoc.2023.day-05
  (:require [aoc.util :as util]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def input (util/read-input))

(def example "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4")

(defn map-fn
  [digits-string]
  (->> (for [[dest-start source-start length] (->> (edn/read-string (str "[" digits-string "]"))
                                                (partition 3))]
         {:dest-start dest-start
          :dest-end (dec (+ dest-start length))
          :source-start source-start
          :source-end (dec (+ source-start length))
          :length length
          :offset (- dest-start source-start)})
    (sort-by :source-start)))

(defn parse-section
  [section]
  (let [[header digits] (str/split section #":")
        map-name (some-> (re-find #"(.+) map" header) second keyword)]
    (case header
      "seeds" {:seeds (edn/read-string (format "[%s]" digits))}
      ("seed-to-soil map"
        "soil-to-fertilizer map"
        "water-to-light map"
        "light-to-temperature map"
        "temperature-to-humidity map"
        "humidity-to-location map"
        "fertilizer-to-water map")
      {map-name (map-fn digits)}
      nil)))

(defn parse-input
  [input]
  (->> (str/split input #"\n\n")
    (map parse-section)
    (apply merge)))

(defn part-1
  [input]
  (let [garden (parse-input input)
        map-order [:seed-to-soil
                   :soil-to-fertilizer
                   :fertilizer-to-water
                   :water-to-light
                   :light-to-temperature
                   :temperature-to-humidity
                   :humidity-to-location]]
    (->> (for [seed (:seeds garden)]
           (reduce
             (fn [x m]
               (let [{:keys [offset] :or {offset 0}}
                     (first
                       (filter
                         #(<= (:source-start %) x (:source-end %))
                         m))]
                 (+ x offset)))
             seed
             (map garden map-order)))
      (sort)
      (first))))

(defn ranges-overlap?
  [r1 r2]
  (not (or (< (:end r1) (:start r2))
         (> (:start r1) (:end r2)))))

(comment
  ; false
  (ranges-overlap? {:start 0 :end 1} {:start 2 :end 3})
  ; true
  (ranges-overlap? {:start 0 :end 1} {:start 1 :end 3})
  (ranges-overlap? {:start 0 :end 1} {:start -1 :end 0})
  (ranges-overlap? {:start 0 :end 1} {:start -1 :end 2}))

(defn range-intersection
  [r1 r2]
  (when (ranges-overlap? r1 r2)
    {:start (apply max (map :start [r1 r2]))
     :end (apply min (map :end [r1 r2]))}))

(comment
  (range-intersection {:start 1 :end 3} {:start 0 :end 4})
  (range-intersection {:start 1 :end 3} {:start 0 :end 2})
  (range-intersection {:start 1 :end 3} {:start 2 :end 4})
  (range-intersection {:start 1 :end 2} {:start 3 :end 4}))

(defn range-difference
  [r1 r2]
  (if (ranges-overlap? r1 r2)
    (filterv some?
      [(when (< (:start r1) (:start r2))
         (assoc r1 :end (min (:end r1) (dec (:start r2)))))
       (when (> (:end r1) (:end r2))
         (assoc r1 :start (max (:start r1) (inc (:end r2)))))])
    [r1]))

(comment
  (range-difference {:start 1 :end 3} {:start 1 :end 2})
  (range-difference {:start 1 :end 3} {:start 2 :end 2})
  (range-difference {:start 1 :end 3} {:start 0 :end 4})
  (range-difference {:start 1 :end 3} {:start 3 :end 4})
  (range-difference {:start 1 :end 3} {:start 4 :end 5}))

(defn range-set-difference
  [s1 s2]
  (set
    (reduce
      (fn [s1 r2]
        (mapcat #(range-difference % r2) s1))
      s1
      s2)))

(comment
  (range-set-difference
    #{{:start 1 :end 2}
      {:start 3 :end 4}}
    [{:start 2 :end 3}
     {:start 3 :end 4}]))

(defn normalize-map
  [g m]
  (update g m (partial map #(assoc %
                              :start (:source-start %)
                              :end (:source-end %)))))

(defn map-layer
  [source-ranges map-ranges]
  (remove nil?
    (concat
      (for [s source-ranges
            {:keys [offset] :as m} map-ranges]
        (some-> (range-intersection s m)
          (update :start + offset)
          (update :end + offset)))
      (range-set-difference source-ranges map-ranges))))

(comment
  (map-layer
    #{{:start 0, :end 105}
      {:start 55, :end 67}}
    '({:dest-start 52,
       :dest-end 99,
       :source-start 50,
       :source-end 97,
       :length 48,
       :offset 2,
       :start 50,
       :end 97}
      {:dest-start 50,
       :dest-end 51,
       :source-start 98,
       :source-end 99,
       :length 2,
       :offset -48,
       :start 98,
       :end 99})))

(defn part-2
  [input]
  (let [map-order [:seed-to-soil
                   :soil-to-fertilizer
                   :fertilizer-to-water
                   :water-to-light
                   :light-to-temperature
                   :temperature-to-humidity
                   :humidity-to-location]
        garden (as-> (parse-input input) $
                 (assoc $ :seed-ranges (->> (partition 2 (:seeds $))
                                         (map (fn [[s l]]
                                                {:start s :end (+ s l -1)}))))
                 (reduce normalize-map $ map-order))]
    (->> (reduce
           map-layer
           (set (:seed-ranges garden))
           (map garden map-order))
      (map :start)
      (sort)
      (first))))
