(ns day12
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day12/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day12/" "input.txt"))

(defn parse-lines
  [lines]
  (->> (map #(filter not-empty (str/split % #"[\s.]")) lines)
       (map #(split-at (dec (count %)) %))
       (map #(vector (first %) (map parse-long (str/split (first (second %)) #","))))))

(defn line-solve
  [patterns numbers]
  (let [[group & rest-groups] patterns]
  (loop [current group
         r-groups rest-groups
         r-nums numbers
         sum 0]
    (cond 
      (nil? current) sum
      (< (count current) number
      :else 0)))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")]
        (map line-solve (parse-lines lines))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]))


(defn expand [[x ys]] (for [y ys] [x y]))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
