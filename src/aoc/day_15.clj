(ns aoc.day-15
  (:require [aoc.util :as util]
            [clojure.data.priority-map :as pm]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input (util/read-input))
(def example "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")
(def start [0 0])

(defn parse-grid
  [s]
  (util/parse-2d-grid
    (fn [p c]
      [p (parse-long (str c))])
    s))

(comment
  (parse-grid example))

(defn neighbors
  [p]
  (for [dp [[-1 0] [0 -1] [1 0] [0 1]]]
    (mapv + p dp)))

(defn step-dijkstra
  [neighbors-fn step-cost-fn state]
  (let [[pos cost] (peek (:frontier state))]
    {:frontier (reduce
                 conj
                 (pop (:frontier state))
                 (->> (neighbors-fn pos)
                      (remove (:visited state))
                      (remove (:frontier state))
                      (map (juxt identity #(+ cost (step-cost-fn pos %))))))
     :visited  (conj (:visited state) (peek (:frontier state)))}))

(defn dijkstra
  [neighbors-fn step-cost-fn origin destination]
  (->> (iterate
         (partial step-dijkstra neighbors-fn step-cost-fn)
         {:frontier (pm/priority-map origin 0)
          :visited {}})
       (filter #((:visited %) destination))
       (first)))

(defn explore*
  [grid state]
  (let [[pos cost] (peek (:frontier state))]
    {:frontier (reduce
                 conj
                 (pop (:frontier state))
                 (->> (neighbors pos)
                      (filter grid)
                      (remove (:visited state))
                      (remove (:frontier state))
                      (map (juxt identity #(+ cost (grid %))))))
     :visited  (conj (:visited state) (peek (:frontier state)))}))

(defn explore
  [grid state]
  (step-dijkstra
    (fn [pos] (filter grid (neighbors pos)))
    (fn [_origin destination] (grid destination))
    state))

(defn dijkstra-lowest-cost
  [start destination grid]
  (let [state {:frontier (pm/priority-map start 0)
               :visited {}}]
    (->> (iterate (partial explore grid) state)
         (keep (fn [state]
                 ((:visited state) destination)))
         (first))))

(defn part-1
  []
  (let [grid (parse-grid input)
        destination [(dec (count (first (str/split-lines input))))
                     (dec (count (str/split-lines input)))]]
    (dijkstra
      (fn [pos] (filter grid (neighbors pos)))
      (fn [_origin destination] (grid destination))
      [0 0]
      destination)))

(def expanded-example "11637517422274862853338597396444961841755517295286\n13813736722492484783351359589446246169155735727126\n21365113283247622439435873354154698446526571955763\n36949315694715142671582625378269373648937148475914\n74634171118574528222968563933317967414442817852555\n13191281372421239248353234135946434524615754563572\n13599124212461123532357223464346833457545794456865\n31254216394236532741534764385264587549637569865174\n12931385212314249632342535174345364628545647573965\n23119445813422155692453326671356443778246755488935\n22748628533385973964449618417555172952866628316397\n24924847833513595894462461691557357271266846838237\n32476224394358733541546984465265719557637682166874\n47151426715826253782693736489371484759148259586125\n85745282229685639333179674144428178525553928963666\n24212392483532341359464345246157545635726865674683\n24611235323572234643468334575457944568656815567976\n42365327415347643852645875496375698651748671976285\n23142496323425351743453646285456475739656758684176\n34221556924533266713564437782467554889357866599146\n33859739644496184175551729528666283163977739427418\n35135958944624616915573572712668468382377957949348\n43587335415469844652657195576376821668748793277985\n58262537826937364893714847591482595861259361697236\n96856393331796741444281785255539289636664139174777\n35323413594643452461575456357268656746837976785794\n35722346434683345754579445686568155679767926678187\n53476438526458754963756986517486719762859782187396\n34253517434536462854564757396567586841767869795287\n45332667135644377824675548893578665991468977611257\n44961841755517295286662831639777394274188841538529\n46246169155735727126684683823779579493488168151459\n54698446526571955763768216687487932779859814388196\n69373648937148475914825958612593616972361472718347\n17967414442817852555392896366641391747775241285888\n46434524615754563572686567468379767857948187896815\n46833457545794456865681556797679266781878137789298\n64587549637569865174867197628597821873961893298417\n45364628545647573965675868417678697952878971816398\n56443778246755488935786659914689776112579188722368\n55172952866628316397773942741888415385299952649631\n57357271266846838237795794934881681514599279262561\n65719557637682166874879327798598143881961925499217\n71484759148259586125936169723614727183472583829458\n28178525553928963666413917477752412858886352396999\n57545635726865674683797678579481878968159298917926\n57944568656815567976792667818781377892989248891319\n75698651748671976285978218739618932984172914319528\n56475739656758684176786979528789718163989182927419\n67554889357866599146897761125791887223681299833479")

(defn expand-grid
  [grid]
  (let [[_xmin xmax] ((juxt first last) (sort (map first (keys grid))))
        [_ymin ymax] ((juxt first last) (sort (map second (keys grid))))]
    (->> (for [x-tile (range 5)
               y-tile (range 5)
               :let [drisk (+ x-tile y-tile)
                     dx (* x-tile (inc xmax))
                     dy (* y-tile (inc ymax))]]
           (do
             (map
               (fn [[p risk]]
                 [(mapv + p [dx dy])
                  (last (take-while pos? (iterate #(- % 9) (+ risk drisk))))])
               grid)))
         (apply concat)
         (into {}))))

(t/deftest expand-input-test
  (t/is (= (parse-grid expanded-example)
           (expand-grid (parse-grid example)))))

(defn part-2
  []
  (let [destination (mapv #(dec (* 5 (inc %))) [(dec (count (first (str/split-lines input))))
                                                (dec (count (str/split-lines input)))])]
    (dijkstra-lowest-cost [0 0] destination (expand-grid (parse-grid input)))))



