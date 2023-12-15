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

(defn reflection?
  [[f s] error] (= error (count (filter false? (map = f s)))))

(defn reflection-point 
  [block error]
  (->> block
       (partition 2 1)
       (keep #(reflection? % error))
       (map-indexed #(vector %1 %2))
       (filter second)
       (map first)))

(defn valid-mirror-point
  [[points block] error]
  (filter (fn [p] 
            (loop [i (dec p)
                   j (inc (inc p))]
              (let [i-line (get block i)
                    j-line (get block j)]
                (cond
                  (or (nil? i-line) (nil? j-line)) true
                  (not (reflection? [i-line j-line] error)) false
                  :else (recur (dec i) (inc j)))))) 
          points))


(defn mirror-vals [blocks error]
  (->> blocks
       (map #(into [] %))
       (map (juxt #(reflection-point % error) identity))
       (filter #(not-empty (first %)))
       (map #(valid-mirror-point % error))
       (filter not-empty)
       (flatten)
       (map inc)))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n\n")
        h (mapv #(str/split % #"\n") lines)
        v (mapv columns h)
        [h-res v-res] (map #(mirror-vals % 0) [h v])]
    (reduce + (concat (map #(* 100 %) h-res) v-res))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n\n")
        h (mapv #(str/split % #"\n") lines)
        v (mapv columns h)
        [h-res v-res] (map #(mirror-vals % 1) [h v])]
    [h-res v-res]))


(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve1 real_input_file)
