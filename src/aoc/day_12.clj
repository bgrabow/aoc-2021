(ns aoc.day-12
  (:require [clojure.string :as str]
            [aoc.util :as util]))

(def example-1 "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def example-2 "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc")
(def example-3 "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW")
(def input (util/read-input))

(defn big-room?
  [s]
  (= s (str/upper-case s)))

(def small-room? (complement big-room?))

(defn input->graph
  [input]
  (-> (->> (str/split-lines input)
           (map #(str/split % #"-"))
           (mapcat (juxt identity (comp vec reverse)))
           (remove (comp #{"start"} second))
           (remove (comp #{"end"} first))
           (group-by first))
      (util/map-vals #(vec (sort (map second %))))))

(defn part-1
  []
  (time (let [graph (input->graph input)]
          (-> ((fn paths-to-end
                 [visited path]
                 (if (= "end" (peek path))
                   [path]
                   (when-let [open-neighbors (->> (get graph (peek path))
                                                  (remove (every-pred visited small-room?))
                                                  (seq))]
                     (mapcat #(paths-to-end (conj visited %) (conj path %)) open-neighbors))))
               #{"start"}
               ["start"])
              (count)))))

(defn part-2
  []
  (time (let [graph (input->graph input)]
          (-> ((fn paths-to-end
                 [visited path visited-twice]
                 #_(prn visited path visited-twice)
                 (if (= "end" (peek path))
                   [path]
                   (when-let [open-neighbors (->> (get graph (peek path))
                                                  (remove #(and (seq visited-twice)
                                                                (small-room? %)
                                                                (visited %)))
                                                  (seq))]
                     (mapcat #(paths-to-end
                                (cond-> visited (small-room? %) (conj %))
                                (conj path %)
                                (cond-> visited-twice (visited %) (conj %)))
                             open-neighbors))))
               #{"start"}
               ["start"]
               #{})
              (count)))))

(defn part-2-early-counting
  []
  (time (let [graph (input->graph input)]
          (-> ((fn num-paths-to-end
                 [visited pos visited-twice]
                 #_(prn visited pos visited-twice)
                 (if (= "end" pos)
                   1
                   (if-let [open-neighbors (->> (get graph pos)
                                                (remove #(and (seq visited-twice)
                                                              (small-room? %)
                                                              (visited %)))
                                                (seq))]
                     (->> (map #(num-paths-to-end
                                  (cond-> visited (small-room? %) (conj %))
                                  %
                                  (cond-> visited-twice (visited %) (conj %)))
                               open-neighbors)
                          (reduce +))
                     0)))
               #{"start"}
               "start"
               #{})))))

(comment
  (time
    (do
      (def num-paths-to-end
        (memoize
          (fn [graph visited pos visited-twice]
            (if (= "end" pos)
              1
              (if-let [open-neighbors (->> (get graph pos)
                                           (remove #(and (seq visited-twice)
                                                         (small-room? %)
                                                         (visited %)))
                                           (seq))]
                (->> (map #(num-paths-to-end
                             graph
                             (cond-> visited (small-room? %) (conj %))
                             %
                             (cond-> visited-twice (visited %) (conj %)))
                          open-neighbors)
                     (reduce +))
                0)))))
      (num-paths-to-end
        (input->graph input)
        #{"start"}
        "start"
        #{}))))
