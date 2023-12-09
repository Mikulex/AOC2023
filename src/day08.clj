(ns day08
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day08/" "demo.txt"))
(def demo_input2_file (str (System/getProperty "user.dir") "/inputs/day08/" "demo2.txt"))
(def demo_input3_file (str (System/getProperty "user.dir") "/inputs/day08/" "demo3.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day08/" "input.txt"))

(defn solve1 
  [file]
  (let [lines (filter not-empty (str/split (slurp file) #"\n"))
        directions (into [] (first lines))
        mappings (into {} (map (fn [[node & roads]] [node roads] ) (map #(re-seq #"[A-Z]{3}" %) (rest lines))))]
    (loop [idx 0
           location "AAA"]
      (let [direction (nth directions (mod idx (count directions)))
            options (get mappings location)]
        (if (= location "ZZZ")
          idx
          (recur (inc idx) (if (= direction \L) (first options) (last options))))))))

(defn first-end-nodes
  [locations
   mappings
   directions]
  (for [location locations]
    (let [[start-location start-idx] location]
      (loop [idx start-idx
             location start-location]
        (if (= \Z (last location))
          [location idx]
          (let [direction (nth directions (mod idx (count directions)))]
            (if (= \L direction)
              (recur (inc idx) (first (get mappings location)))
              (recur (inc idx) (last (get mappings location))))))))))

(defn lcm
  [numbers]
  (let [max-num (apply max numbers)]
    (loop [n 2]
      (let [mult (* n max-num)]
        (if (every? zero? (map #(mod mult %) numbers))
                    mult
                    (recur (inc n)))))))
(defn solve2
  [file]
  (let [lines (filter not-empty (str/split (slurp file) #"\n"))
        directions (into [] (first lines))
        mappings (into {} (map (fn [[node & roads]] [node roads] ) (map #(re-seq #"[A-Z1-9]{3}" %) (rest lines))))
        start-nodes (map vector (filter #(= \A (last %)) (map first mappings)) (repeat 0))]
    (lcm (map last (first-end-nodes start-nodes mappings directions)))))

(solve1 demo_input_file)
(solve1 demo_input2_file)
(solve1 real_input_file)

(solve2 demo_input3_file)
(time (solve2 real_input_file))
