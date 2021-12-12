(ns aoc.day-10-jose
  (:require [aoc.util :as util]
            [clojure.string :as str]))

(def input (slurp "resources/input/day_10.txt"))

(def example-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]\n")

;; Find the first corrupted character in each line.
;; Score that character with the following scores:
;): 3 points.
;]: 57 points.
;}: 1197 points.
;>: 25137 points.
;; Add up all the scores.

;; Split up input by lines into vector/list

(def matched-delimiters
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(defn opening-delimiter?
  [c]
  (some? (matched-delimiters c)))

(defn find-corrupted-character
  [line]
  (let [result (reduce
                 (fn [stack c]
                   (cond
                     (= c (matched-delimiters (peek stack))) (pop stack)

                     (opening-delimiter? c) (conj stack c)

                     :else (reduced c)))
                 [] line)]
    (when (char? result)
      result)))

(defn find-corrupted-character-xd
  [line]
  (transduce
    (map identity)
    (fn
      ([stack c]
       (prn "reducing" stack c)
       (cond
         (= c (matched-delimiters (peek stack))) (pop stack)

         (opening-delimiter? c) (conj stack c)

         :else (reduced c)))
      ([acc]
       (prn "completing" acc (class acc))
       (if (reduced? acc)
         acc
         nil)))
    []
    line))

;; reducing fn: Compares head of stack to the next character.
;; Adds opening delimiters to the stack.
;; Removes pairs of matched delimiters.
;; (reduced) when encountering an invalid character

(comment
  (let [stack []]
    ;(pop stack)
    ;(peek stack)
    (-> (conj stack 5)
        (peek))))

(comment
  (find-corrupted-character "([<}") ; => \}
  (find-corrupted-character "()") ; => nil
  (find-corrupted-character "(") ; => nil
  ,)

(def corrupted-character->score
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(comment
  (->> (str/split-lines input)
       (keep find-corrupted-character)
       (reduce (fn [sum c] (+ sum (corrupted-character->score c))) 0)))

(defn part-1
  []
  (time (->> (str/split-lines input)
             (transduce
               (comp
                 (keep find-corrupted-character)
                 (map corrupted-character->score))
               +))))


;; Each line, find the first corrupted character
;; * Maintain a stack of opening delimiters
;; * When encountering a closing delimiter,
;;   * check if it matches the head of the stack
;;   * If yes, remove the head of the stack and discard the


;; [({(<(())[]>[[{[]{<()<>>
;; [(])
;; [()]
;; [)(]


;; [()]
;;   [()]
;; [ ()]
;; [( )]
;; [  ]
;; _


;; [)(]
;; [ )(]
;;

;; reduce
;; acc: stack '()
;; coll: list of characters in the line
;; reducing fn: Compares head of stack to the next character.
;; Adds opening delimiters to the stack.
;; Removes pairs of matched delimiters.
;; (reduced) when encountering an invalid character
