(ns day05
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day05/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day05/" "input.txt"))

(defn parse-map-sorted
  [line]
  (as-> (re-find #"(?![[a-z]-\s\:]+)([\d\ss]+)" line) l
    (last l)
    (str/split l #"\n")
    (map #(str/split % #" ") l)
    (map #(map parse-long %) l)
    (sort #(< (second %1) (second %2)) l)))

(defn parse-seeds
  [line]
  (map parse-long 
       (filter not-empty 
               (str/split (re-find #"[\s\d]+" line) #" "))))

(defn parse-input 
  [lines]
  (map-indexed 
      (fn [idx line]
        (if (= idx 0)
          (parse-seeds line)
          (parse-map-sorted line)))
      lines))

(defn correct-mapping
  [mappings seed]
  (first 
    (filter (fn [mapping]
              (let [[d s r] mapping]
                (and (> (+ s r) seed)
                     (<= s seed))))
            mappings)))

(defn pass-through 
  [mappings seed]
  (let [[d s r] (correct-mapping mappings seed)]
    (cond
      (nil? s) seed
      (< (+ s r) seed) seed
      :else (+ d (- seed s)))))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n\n")
        [seeds & maps] (parse-input lines)]
    (apply min (loop [remaining maps
           values seeds]
      (if (empty? remaining)
        values
        (recur (rest remaining)
               (map #(pass-through (first remaining) %) values)))))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    lines))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)


