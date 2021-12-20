(ns aoc.day-20
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def sample
  (let [[decoder-lines image] (str/split "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\n#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\n.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\n.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\n.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\n...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\n..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###" #"\n\n")]
    (str (str/join (str/split-lines decoder-lines)) "\n\n" image)))
(def input (util/read-input))

(def text->bin
  {\# 1 \. 0})

(defn parse-input
  [input]
  (let [[decoder _ & image] (str/split-lines input)
        image (mapv (comp vec (partial map text->bin)) image)]
    {:decoder (mapv text->bin decoder)
     :image (into {} (for [y (range)
                           :while (get image y)
                           x (range)
                           :while (get-in image [y x])
                           :when (not (zero? (get-in image [y x])))]
                       [[x y] (get-in image [y x])]))}))

(defn neighborhood
  [p]
  (for [dy [-1 0 1]
        dx [-1 0 1]]
    (mapv + p [dx dy])))

(comment
  (neighborhood [0 0]))

(defn bin-digits->long
  [digits]
  (Long/valueOf (apply str digits) 2))

(defn print-image-range
  [image range-x range-y]
  (println)
  (doseq [y range-y]
    (println
      (apply str
             (for [x range-x]
               ({1 \# 0 \.} (get image [x y] 0))))))
  image)

(defn print-image
  [image]
  (println)
  (let [[ymin ymax] ((juxt first last) (sort (map second (keys image))))
        [xmin xmax] ((juxt first last) (sort (map first (keys image))))]
    (print-image-range image (range xmin (inc xmax)) (range ymin (inc ymax)))
    #_(doseq [y (range ymin (inc ymax))]
        (println (apply str (for [x (range xmin (inc xmax))]
                              ({1 \# 0 \.} (get image [x y] 0)))))))
  image)

(defn enhancement-bits
  [image center]
  (->> (neighborhood center)
       (map (fn [p] (get image p 0)))))

(defn enhancement-index
  [image center]
  (bin-digits->long (enhancement-bits image center)))

(defn debug-print
  [x]
  (println x)
  x)

(defn step
  [decoder image]
  (->> (mapcat neighborhood (keys image))
       (set)
       (map (juxt identity #(->> (neighborhood %)
                                 (map (fn [p] (get image p 0)))
                                 (bin-digits->long)
                                 (debug-print)
                                 (get decoder))))
       (remove (comp zero? second))
       (into {})))

(defn weight-contributions
  [p]
  (zipmap (neighborhood p) (iterate #(* 2 %) 1)))

(defn step-2
  [decoder image]
  (-> (->> (keys image)
           (map weight-contributions)
           (apply merge-with +))
      (util/map-vals (partial get decoder))
      (->> (remove (comp zero? second))
           (into {}))))

(defn epileptic-2-step
  [decoder image]
  (if (= 1 (get decoder 0))
    (let [inverse-decoder (vec (reverse decoder))]
      (-> (->> (keys image)
               (map weight-contributions)
               (apply merge-with +))
          (util/map-vals (partial get decoder))
          (->> (filter (comp zero? second))
               (into {})
               (keys)
               (map weight-contributions)
               (apply merge-with +))
          (util/map-vals (partial get inverse-decoder))
          (->> (remove (comp zero? second))
               (into {}))))
    (step-2 decoder (step-2 decoder image))))

(defn top-left-square
  [n image]
  (let [[ymin] ((juxt first last) (sort (map second (keys image))))
        [xmin] ((juxt first last) (sort (map first (keys image))))]
    (->> (filter (comp #(and (< (first %) (+ n xmin))
                             (< (second %) (+ n ymin))) first) image)
         (into {}))))

(defn some-scans
  [image range-x range-y]
  (for [y range-y
        x range-x]
    [[x y] (enhancement-bits image [x y])]))

(comment
  (let [{:keys [decoder image]} (parse-input input)]
    (->> (iterate (partial step decoder) image)
         (take 3)
         ;(last)
         ;(count))))
         ;(map (partial top-left-square 5))
         (map #(print-image-range % (range -2 5) (range -2 5)))
         (map #(some-scans % (range -2 5) (range -2 5))))))
  ;(enhancement-index image [2 2]))
;(map print-image)))

(comment
  (let [{:keys [decoder image]} (parse-input input)]
    (float (/ (count (step decoder image))
              (count image)))
    (float (/ (count (filter #{1} decoder)) (count (filter #{0} decoder))))
    (print-image (top-left-square 10 (step decoder image)))))

(comment
  (count (mapcat neighborhood (keys (:image (parse-input sample))))))

(defn long->bin-digits
  [x]
  (map (comp parse-long str) (format "%09d" (parse-long (Long/toBinaryString x)))))

(comment
  (= (into {} (map-indexed #(vector (long->bin-digits %1) %2) (:decoder (parse-input input))))
     (map-indexed #(vector (long->bin-digits %1) %2) (:decoder (parse-input sample)))))


(comment
  (let [{:keys [decoder image]} (parse-input input)]
    (->> (take 3 (iterate (partial step-2 decoder) image))
         (last))
    (get decoder 511)))

(defn part-1
  []
  (let [{:keys [decoder image]} (parse-input input)]
    (->> (take 2 (iterate (partial epileptic-2-step decoder) image))
         (last)
         (count))))
