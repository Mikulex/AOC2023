(ns day04
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day04/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day04/" "input.txt"))

(defn parse-game 
  [line]
  (as-> (re-seq #"(Card\s+\d{1,3}:)([\d\s|]+)" line) l
    (flatten l)
    (last l)
    (str/split l #"\|")
    (map #(str/split % #" ") l)
    (map #(filter not-empty %) l)
    (map #(map parse-long %) l)
    (map #(into #{} %) l)))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    (->> (map parse-game lines)
         (map #(apply set/intersection %))
         (filter not-empty)
         (map count)
         (map #(.pow 2M (dec %)))
         (reduce +))))
        
(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
