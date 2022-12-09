(ns aoc.2022.day-07
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(def input (slurp "resources/2022/input07.txt"))

(defn parse-instruction
  [s]
  (let [[op & args] (str/split s #" ")]
    (case op
      "$" (case (first args)
            "cd" {:op :cd :dest (second args)}
            "ls" {:op :ls})
      "dir" {:op :dir :dirname (first args)}
      {:op       :file
       :filename (first args)
       :size     (parse-long op)})))

(defn parse-instructions
  [s]
  (->> (str/split-lines s)
       (map parse-instruction)))

(defn cd
  [state dest]
  (case dest
    ".." (-> state
             #_(assoc-in [:files (peek (:path state))]
                 (->> (get-in state [:dirs (peek (:path state))])
                      (map #(get-in state [:files %]))
                      (reduce +)))
             (update :path pop))
    (update state :path (fnil conj []) dest)))

(defn file
  [state filename size]
  (-> state
      (update :files assoc (conj (:path state) filename) size)
      (update-in [:dirs (:path state)] (fnil conj #{}) filename)
      #_(assoc-in (concat [:tree] (reverse (:path state)) [filename]) {:filename filename :size size})))

(defn dir
  [state dirname]
  (-> state
      (update-in [:dirs (:path state)] (fnil conj #{}) dirname)
      #_(assoc-in (concat [:tree] (reverse (:path state)) [:dirname]) dirname)))

(defn operate
  [state {:keys [op filename dirname dest size]}]
  (case op
    :dir (dir state dirname)
    :cd (cd state dest)
    :ls state
    :file (file state filename size)))

(defn return-to-root
  [state]
  (let [depth (count (:path state))]
    (reduce operate state (repeat depth {:op :cd :dest ".."}))))

(defn dir?
  [state filename]
  (boolean (get-in state [:dirs filename])))

(defn file?
  [state filename]
  (not (boolean (get-in state [:dirs filename]))))

(defn dirsize
  [state dirpath]
  (->> (for [filename (get-in state [:dirs dirpath])]
         (or (get-in state [:files (conj dirpath filename)])
             (dirsize state (conj dirpath filename))))
       (reduce +)))

#_(defn solve-1
    [s]
    (let [state (return-to-root (reduce operate {} (parse-instructions s)))]
      #_(->> (keys (:dirs state))
             (random-sample 0.5)
             (map (partial dirsize state))
             (filter #(<= % 100000))
             (reduce +))
      (->> (walk/postwalk
             (fn [node]
               (if (and (map? node)
                     (not (:size node)))
                 (assoc node :size (reduce + (keep :size (vals node))))
                 node))
             (:tree state))
           (tree-seq
             :size
             #(filter :dirname (vals %)))
           (filter #(<= (:size %) 100000))
           (map :size)
           (reduce +))
      (->> (for [[filename size] (:files state)
                 :when (and (<= size 100000)
                            (dir? state filename))]
             [filename size]))))

(defn solve-1
  [s]
  (let [state (reduce operate {} (parse-instructions s))]
    (->> (for [[dirpath _contents] (:dirs state)
               :let [size (dirsize state dirpath)]
               :when (<= size 100000)]
           size)
         (reduce +))))

(comment
  (solve-1 input))
