(ns aoc.2022.day-07
  (:require [clojure.string :as str]))

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
    ".." (update state :path pop)
    (update state :path (fnil conj []) dest)))

(defn file
  [state filename size]
  (-> state
      (update :files assoc (conj (:path state) filename) size)
      (update-in [:dirs (:path state)] (fnil conj #{}) filename)))

(defn dir
  [state dirname]
  (-> state
      (update-in [:dirs (:path state)] (fnil conj #{}) dirname)))

(defn operate
  [state {:keys [op filename dirname dest size]}]
  (case op
    :dir (dir state dirname)
    :cd (cd state dest)
    :ls state
    :file (file state filename size)))

(defn dirsize
  [state dirpath]
  (->> (for [filename (get-in state [:dirs dirpath])]
         (or (get-in state [:files (conj dirpath filename)])
             (dirsize state (conj dirpath filename))))
       (reduce +)))

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

(def total-disk-space 70000000)
(def required-disk-space 30000000)

(defn solve-2
  [s]
  (let [state (reduce operate {} (parse-instructions s))
        total-used-space (dirsize state ["/"])
        big-enough-dirsizes (sort (for [[dirpath _contents] (:dirs state)
                                        :let [size (dirsize state dirpath)]
                                        :when (>= (+ (- total-disk-space total-used-space)
                                                     size)
                                                  required-disk-space)]
                                    size))]
    (first big-enough-dirsizes)))

(comment
  (solve-2 input))
