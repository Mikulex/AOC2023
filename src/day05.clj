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
  (map parse-long (filter not-empty (str/split (re-find #"[\s\d]+" line) #" "))))

(defn parse-input 
  [lines]
  (map-indexed (fn [idx line]
                 (if (= idx 0)
                   (parse-seeds line)
                   (parse-map-sorted line)))
               lines))

(defn correct-mapping
  [mappings seed]
  (first (filter (fn [mapping]
                   (let [[_ s r] mapping]
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
    (apply min 
           (loop [remaining maps
                  values seeds]
             (if (empty? remaining)
               values
               (recur (rest remaining)
                      (map #(pass-through (first remaining) %) values)))))))

(defn pass-through-ranges 
  "Check for overlaps between mappings and a seed range, split seed-ranges accordingly, 
  update seed ranges affected by mappings and return a list of seed ranges"
  [mappings seed-range]
  (loop [remaining mappings
         mapped-vals []
         [idx offset] seed-range]
    (let [[d s r] (first remaining)]
      (cond
        (nil? s) (conj mapped-vals [idx offset])
        (= offset 0) mapped-vals
        (<= (+ idx offset) s) (conj mapped-vals [idx offset])
        (>= idx (+ s r)) (recur (rest remaining)
                                mapped-vals
                                [idx offset])
        (and (>= idx s) (<= (+ idx offset) (+ s r))) (conj mapped-vals [(+ d (- idx s)) offset])
        (< idx s) (recur remaining
                         (conj mapped-vals [idx (- s idx)])
                         [s (- offset (- s idx))])
        :else (recur (rest remaining)
                     (conj mapped-vals [(+ d (- r (- (+ s r) idx))) (- (+ s r) idx)])
                     [(+ s r) (- offset (- (+ s r) idx))])))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n\n")
        [seed-ranges & maps] (parse-input lines)
        seeds (partition 2 seed-ranges)]
    (apply min (map first 
                    (loop [remaining maps
                           values seeds]
                      (if (empty? remaining)
                        values
                        (recur (rest remaining)
                               (mapcat #(pass-through-ranges (first remaining) %) values))))))))

(mapcat #(vec [% %]) [1 2 3])

(defn amogus [b] (fn [a] (+ a b)))
((amogus 5) 1)

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)


