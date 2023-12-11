(ns day11
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day11/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day11/" "input.txt"))
(declare vertical-empty-spaces horizontal-empty-spaces find-indeces galaxies update-coords expand)

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")
        v-empty (vertical-empty-spaces lines)
        h-empty (horizontal-empty-spaces lines)
        galaxies (map #(update-coords % h-empty v-empty 2) (mapcat expand (galaxies lines)))]
    (/ (reduce + (for [a galaxies b galaxies :when (not (= a b))]
                   (reduce + (map abs (map - a b))))) 2)))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")
        v-empty (vertical-empty-spaces lines)
        h-empty (horizontal-empty-spaces lines)
        galaxies (map #(update-coords % h-empty v-empty 1000000) (mapcat expand (galaxies lines)))]
    (/ (reduce + (for [a galaxies b galaxies :when (not (= a b))]
                   (reduce + (map abs (map - a b))))) 2)))


(defn expand [[x ys]] (for [y ys] [x y]))

(defn vertical-empty-spaces
  [lines]
  (->> (for [y (range (count lines)) x (range (count (first lines)))](get-in lines [x y]))
       (partition (count lines))
       (map-indexed #(vector %1 %2))
       (filter #(nil? (str/index-of %1 "#")))
       (map first)))

(defn horizontal-empty-spaces
  [lines]
  (map first (filter #(nil? (str/index-of %1 "#")) (map-indexed #(vector %1 %2) lines))))

(defn galaxies [lines]
  (filter #(not-empty (second %)) (map-indexed #(vector %1 (find-indeces %2)) lines)))

(defn update-coords 
  [[x y] h-empty v-empty times]
  (let [add-x (* (dec times) (count (first (split-with (partial >= x) h-empty))))
        add-y (* (dec times) (count (first (split-with (partial >= y) v-empty))))]
    (vector (+ x add-x) (+ y add-y))))

(defn find-indeces
  [line]
  (loop [found []
         remaining line
         idx 0]
    (let [found-idx (str/index-of remaining "#")]
      (if (empty? remaining) found
        (if (nil? found-idx) found 
          (recur (conj found (+ idx found-idx))
                 (subs remaining (inc found-idx))
                 (+ idx (inc found-idx))))))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
