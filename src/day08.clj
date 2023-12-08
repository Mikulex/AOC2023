(ns day08
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day08/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day08/" "input.txt"))
(doc partition)

(defn solve1 
  [file]
  (let [lines (filter not-empty (str/split (slurp file) #"\n"))
        directions (flatten (repeat (into [] (first lines))))
        mappings (into {} (map (fn [[node & roads]] [node roads] ) (map #(re-seq #"[A-Z]{3}" %) (rest lines))))]
    (loop [idx 0
           location "AAA"]
      (let [direction (nth directions idx)
            options (get mappings location)]
      (if (= location "ZZZ")
        idx
    (recur (inc idx) (if (= direction \L) (first options) (last options))))))))

(defn solve2
  [file]
  (let [lines (filter not-empty (str/split (slurp file) #"\n"))
        directions (flatten (repeat (into [] (first lines))))
        mappings (into {} (map (fn [[node & roads]] [node roads] ) (map #(re-seq #"[A-Z]{3}" %) (rest lines))))
        start-nodes (filter #(= \A (last %)) (map first mappings))]
    start-nodes))

     
(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
