(ns day06
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day06/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day06/" "input.txt"))

(defn parse-races 
  [lines]
  (let [[times distances] (->> (map #(re-find #"[\d\s]+" %) lines)
                              (map #(str/split % #"\s+"))
                              (map #(filter not-empty %))
                              (map #(map parse-long %)))]
    (map vector times distances)))

(defn parse-as-single-race
  [lines]
  (->> (map #(re-find #"[\d\s]+" %) lines)
                              (map #(str/split % #"\s+"))
                              (map #(filter not-empty %))
                              (map str/join)
                              (map parse-long)))

(defn get-possible-wins 
  [[t d]]
  (filter #(> % d) 
          (for [held (range t)]
            (* held (- t held)))))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    (apply * (map count (map #(get-possible-wins %) (parse-races lines))))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]
        (count (get-possible-wins (parse-as-single-race lines)))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)

