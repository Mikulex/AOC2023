(ns day13
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day13/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day13/" "input.txt"))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n\n")]
    (->> lines 
         (map #(str/split % #"\n"))
         (map (fn [coll] (map-indexed #(vector %1 %2) coll)))
         (map (fn [coll] (sort-by #(second %) coll)))
         (map (fn [coll] (partition-by #(second %) coll)))
         (map (fn [coll] (filter #(= 2 (count %)) coll))))))


(sort-by #(second %) [[0 "333"] [1 "111"] [2 "333"]])

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    ))

(solve1 demo_input_file)
((([2 "##......#"] [3 "##......#"])
  ([1 "..#.##.#."] [4 "..#.##.#."]))

 (([3 "#####.##."] [4 "#####.##."])
  ([1 "#....#..#"] [6 "#....#..#"])
  ([2 "..##..###"] [5 "..##..###"])))
(solve1 real_input_file)

(solve2 demo_input_file)
(time (solve2 real_input_file))
