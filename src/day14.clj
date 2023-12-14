(ns day14
  (:require clojure.repl) 
  (:require [clojure.string :as str]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day14/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day14/" "input.txt"))

(defn transpose
  [lines]
  (map str/join (partition (count lines)
    (for [x (range (count (first lines))) y (range (count lines))] 
    (get-in lines [y x])))))

(defn move-rocks [lines]
  (->> lines
         (map #(partition-by (partial = \#) %))
         (map (fn [portion] (map #(sort %) portion)))
         (map (fn [portion] (map #(reverse %) portion)))
         (map #(apply concat %))
         (map #(apply str %))
         (vec)))

(defn solve1 
  [file]
  (let [lines (transpose (str/split (slurp file) #"\n"))]
    (reduce + (map-indexed (fn [idx freqs] (* (- (count lines) idx) 
                                    (or (get freqs \O) 0)))
                 (map frequencies (transpose (move-rocks lines)))))))
(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve1 real_input_file)
