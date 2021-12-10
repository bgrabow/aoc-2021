(ns aoc.day-10
  (:require [aoc.util :as util]
            [clojure.string :as str]
            [aoc.day-07 :as day-07]))

(def input (util/read-input))

(def example-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")

(def matched-delimiters
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(defn opening-delimiter?
  [c]
  (some? (matched-delimiters c)))

(defn first-corrupted-character
  [s]
  (let [c (reduce
            (fn [stack c]
              (cond
                (= (matched-delimiters (peek stack)) c) (pop stack)

                (opening-delimiter? c) (conj stack c)

                :else (reduced c)))
            []
            s)]
    (when (char? c)
      c)))

(def corrupted-character->score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn part-1
  []
  (->> (str/split-lines input)
       (keep first-corrupted-character)
       (map corrupted-character->score)
       (reduce +)))

(def completing-character->score
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn completing-string-score
  [s]
  (reduce (fn [acc x] (+ x (* 5 acc))) (map completing-character->score s)))

(defn consume-matched-delims
  [s]
  (reduce
    (fn [stack c]
      (cond
        (= (matched-delimiters (peek stack)) c) (pop stack)

        (opening-delimiter? c) (conj stack c)

        :else (reduced nil)))
    []
    s))

(defn part-2
  []
  (->> (str/split-lines input)
       (keep consume-matched-delims)
       (map reverse)
       (map #(map matched-delimiters %))
       (map completing-string-score)
       (day-07/median)))
