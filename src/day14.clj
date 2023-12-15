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

(def move-rocks (memoize (fn [lines orientation direction]
  (->> (orientation lines)
         (map #(partition-by (partial = \#) %))
         (map (fn [portion] (map #(sort %) portion)))
         (map (fn [portion] (map #(direction %) portion)))
         (map #(apply concat %))
         (map #(apply str %))
         (vec)
         (orientation)))))

(defn north [lines] (move-rocks lines transpose reverse))
(defn south [lines] (move-rocks lines transpose identity))
(defn west [lines] (move-rocks lines identity reverse))
(defn east [lines] (move-rocks lines identity identity))

(def spin-cycle (memoize (fn [l n]
  (loop [lines l
         iters n
         last-iter []]
    (cond 
      (zero? iters) lines
      (= last-iter lines) lines
      :else (recur (east (south (west (north lines)))) (dec iters) lines))))))

(defn calc-load [lines]
  (reduce + (map-indexed (fn [idx freqs] 
                           (* (- (count lines) idx) (or (get freqs \O) 0)))
                         (map frequencies lines))))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    (calc-load (north lines))))

(defn loop-points [lines]
  (loop [idx 0
         calculated {}]
    (let [nth-cycle (spin-cycle lines idx)]
      (if (contains? calculated nth-cycle) [(get calculated nth-cycle) (- idx (get calculated nth-cycle))]
        (recur (inc idx) (assoc calculated nth-cycle idx))))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")
        iters 1000000000
        [idx len] (loop-points lines)]
    (prn idx len)
    (calc-load (spin-cycle lines (+ idx (mod (- iters idx) len))))))

(solve1 demo_input_file)
(solve1 real_input_file)

(time (solve2 demo_input_file))
(time (solve2 real_input_file))
