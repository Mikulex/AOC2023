(ns day13
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day13/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day13/" "input.txt"))

(defn columns
  [lines]
  (map str/join (partition (count lines)
    (for [x (range (count (first lines))) y (range (count lines))] 
    (get-in lines [y x])))))

(defn reflection-point 
  [block]
  (->> block
       (partition 2 1)
       (keep #(apply = %))
       (map-indexed #(vector %1 %2))
       (filter second)
       (map first)))

(defn valid-mirror-point
  [[points block]]
  (filter (fn [p] 
            (loop [i p
                   j (inc p)]
              (let [i-line (get block i)
                    j-line (get block j)]
                (cond
                  (or (nil? i-line) (nil? j-line)) true
                  (not= i-line j-line) false
                  :else (recur (dec i) (inc j)))))) 
          points))


(defn mirror-vals [blocks]
  (->> blocks
       (map #(into [] %))
       (map (juxt reflection-point identity))
       (filter #(not-empty (first %)))
       (map valid-mirror-point)
       (filter not-empty)
       (flatten)
       (map inc)))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n\n")
        h (mapv #(str/split % #"\n") lines)
        v (mapv columns h)
        [h-res v-res] (map mirror-vals [h v])]
    (reduce + (concat (map #(* 100 %) h-res) v-res))))
    
(solve1 demo_input_file)
(solve1 real_input_file)
