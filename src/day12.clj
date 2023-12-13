(ns day12
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day12/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day12/" "input.txt"))

(defn parse-lines
  [lines]
  (->> (map #(filter not-empty (str/split % #"[\s]")) lines)
       (map #(split-at (dec (count %)) %))
       (map #(vector (str/join "." (first %)) (map parse-long (str/split (first (second %)) #","))))))

(defn replace-first [s c] ( str/replace-first s #"." (str c)))


(def line-solve
  (memoize (fn [p n r]
    (let [numbers-left (> (reduce + n) 0)
          chars-left (not (nil? (first p)))]
      (cond 
        (and (not chars-left) (not numbers-left)) 1
        (and (not chars-left) numbers-left) 0
        (and (not (nil? (str/index-of p "#"))) (not numbers-left)) 0
        (and (nil? (str/index-of p "#")) (not numbers-left)) 1
        (and (= (first p) \#) (= (first n) 0)) 0
        (= (first p) \#) (recur (subs p 1 (count p)) (conj (rest n) (dec (first n))) true)
        (and (= (first p) \.) (> (first n) 0) r) 0
        (and (= (first p) \.) (= (first n) 0)) (recur (subs p 1 (count p)) (rest n) false)
        (= (first p) \.) (recur (subs p 1 (count p)) n false)
        (= (first p) \?) (+ (line-solve (replace-first p \#) n true) (line-solve (replace-first p \.) n r))
        :else (prn "unmatched case!" p n))))))

(defn solve1 
  [file]
  (let [lines (parse-lines (str/split (slurp file) #"\n"))]
    (reduce + (map #(line-solve (first %1) (second %1) (= (first %) \#)) lines))))

(defn- repeated-lines [lines]
  (map #(vector (str/join "?" (repeat 5 (first %))) (apply concat (repeat 5 (second %)))) lines))

(defn solve2
  [file]
  (let [lines (repeated-lines (parse-lines (str/split (slurp file) #"\n")))]
    (reduce + (pmap #(line-solve (first %1) (second %1) (= (first %) \#)) lines))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(time (solve2 real_input_file))
