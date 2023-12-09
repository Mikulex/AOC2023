(ns day09
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day09/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day09/" "input.txt"))
(declare sub-numseqs next-subseq next-number previous-number)

(defn solve1 
  [file]
  (let [lines (map #(map parse-long %) (map #(str/split % #" ") (str/split (slurp file) #"\n")))]
    (reduce + (map next-number (map sub-numseqs lines)))))

(defn solve2
  [file]
  (let [lines (map #(map parse-long %) (map #(str/split % #" ") (str/split (slurp file) #"\n")))]
    (reduce + (map previous-number (map sub-numseqs lines)))))

(defn sub-numseqs
  [numseq]
  (loop 
    [currentseq numseq
     result [numseq]]
    (if (every? zero? currentseq) result
      (let [new-subseq (next-subseq currentseq)]
        (recur new-subseq (conj result new-subseq))))))

(defn next-subseq
  [numseq]
  (map #(- (second %) (first %)) (partition 2 1 numseq)))

(defn next-number
  [sub-numseqs]
  (reduce + (map last sub-numseqs)))

(defn previous-number
  [sub-numseqs]
  (reduce #(- %2 %1) (reverse (map first sub-numseqs))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
