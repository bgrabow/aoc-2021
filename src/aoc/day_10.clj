(ns aoc.day-10
  (:require [aoc.util :as util]
            [clojure.string :as str]))

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
