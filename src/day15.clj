(ns day15
  (:require clojure.repl) 
  (:require [clojure.string :as str]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day15/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day15/" "input.txt"))

(defn helper-hash [line]
  (loop [sum 0
         [c & r] line]
    (if (nil? c) sum (recur (mod (* (+ (int c) sum) 17) 256) r))))


(defn solve1 
  [file]
  (let [lines (str/split (str/replace (slurp file) "\n" "") #",")]
    (reduce + (map helper-hash lines))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve1 real_input_file)
