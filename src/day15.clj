(ns day15
  (:require clojure.repl) 
  (:require [clojure.string :as str]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day15/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day15/" "input.txt"))

(defn helper-hash [line]
  (loop [sum 0
         [c & r] line]
    (if (nil? c) sum (recur (mod (* (+ (int c) sum) 17) 256) r))))

(defn update-lens [box new-lens]
  (vec (for [current box]
         (if (= (first current) (first new-lens)) new-lens current))))

(defn remove-lens [boxes hash label]
  (update boxes hash (fn [content] (vec (remove #(= (first %) label) content)))))

(defn contains-lens? [label boxes hash]
  (seq (filter #(= (first %) label) (boxes hash))))

(defn follow-instruction [boxes instruction] 
    (let [[label fl :as new] instruction
          hash (helper-hash label)]
      (cond
        (nil? instruction) boxes 
        (nil? fl) (remove-lens boxes hash label)
        (contains-lens? label boxes hash) (update boxes hash #(update-lens % new))
        :else (update boxes hash #(conj (vec %) new)))))

(defn solve1 
  [file]
  (let [lines (str/split (str/replace (slurp file) "\n" "") #",")]
    (reduce + (map helper-hash lines))))

(defn solve2
  [file]
  (let [lines (str/split (str/replace (slurp file) "\n" "") #",")]
    (->> lines
         (map #(str/split % #"[-=]"))
         (reduce follow-instruction {})
         (reduce-kv (fn [m k v]
                      (assoc m k 
                             (map #(* (inc k) (parse-long (last %)))
                                  v))) {})
         vals
         (map (fn [coll] (map-indexed #(* (inc %1) %2) coll)))
         flatten
         (reduce +))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
