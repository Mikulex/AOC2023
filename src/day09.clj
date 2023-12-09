(ns day09
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day09/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day09/" "input.txt"))
(declare sub-numseqs next-subseq next-number)

(defn solve1 
  [file]
  (let [lines (map #(map parse-long %) (map #(str/split % #" ") (str/split (slurp file) #"\n")))]
    (reduce + (map next-number (map sub-numseqs lines)))))

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
  (loop [currentseq numseq
         result []]
    (if (or (empty? currentseq) (= (count currentseq) 1)) result
      (recur (rest currentseq) (conj result (- (second currentseq) (first currentseq)))))))

(defn next-number
  [sub-numseqs]
  (reduce + (map first (map reverse sub-numseqs))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
