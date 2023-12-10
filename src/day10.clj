(ns day10
  (:require clojure.repl) 
  (:require
    [clojure.string :as str]))
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day10/" "demo.txt"))
(def demo_input_file2 (str (System/getProperty "user.dir") "/inputs/day10/" "demo2.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day10/" "input.txt"))
(declare surrounding-pipes surrounding-pipes-init valid-connection? pipe-loop)

(defrecord pipeinfo [coord pipe])

(def pipe-to-dir
  {:up #{\| \F \7 \S}
   :down #{\| \L \J \S}
   :left #{\F \- \L \S}
   :right #{\J \7 \- \S}})

(def opposite
  {:up :down
   :left :right
   :right :left
   :down :up})

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")
        loop-start (->pipeinfo (flatten (filter #(number? (second %)) (map-indexed #(vector %1 (str/index-of %2 "S")) lines))) \S)]
    (/ (count (pipe-loop loop-start lines)) 2)))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")
        loop-start (->pipeinfo (flatten (filter #(number? (second %)) (map-indexed #(vector %1 (str/index-of %2 "S")) lines))) \S)
        loop-pipes (pipe-loop loop-start lines)
        max-x (count lines)
        max-y (count (first lines))]
    (loop [current-location (->pipeinfo [0 0] (get-in lines [0 0]))
           current-area []
           visited #{}
           enclosed []
           other-areas []
           next-locations []
           not-visited-coords (remove #(contains? loop-pipes %) (into #{} (for [a (range max-x) b (range max-y)] (->pipeinfo [a b] (get-in lines [a b])))))
           is-enclosed? true]
      (prn current-location)
      (if (empty? next-locations)
        (if (empty? not-visited-coords) 
          [enclosed other-areas]
          (recur 
            (first not-visited-coords) 
            []
            (conj visited current-location)
            (if is-enclosed? (concat current-area enclosed) enclosed)
            (if is-enclosed?  other-areas (concat current-area other-areas))
            next-locations 
            (remove #(= current-location %) not-visited-coords)
            true))
        (recur 
          (first next-locations)
          (conj current-area current-location)
          (conj visited current-location)
          enclosed
          other-areas
          (rest (conj next-locations (filter #(nil? (:pipe %)) (surrounding-pipes-init current-location lines))))
          (remove #(= not-visited-coords %) current-location)
          (not (or (= (first current-location) 0)
                   (= (second current-location) 0)
                   (= (first current-location) (dec max-x))
                   (= (second current-location) (dec max-y)))))))))

(defn pipe-loop
  [loop-start 
   lines]
  (loop [pipe loop-start
         visited (hash-set loop-start)]
    (let [pipes (set/difference (surrounding-pipes pipe lines) visited)]
      (if (set/subset? pipes visited)
        visited
        (recur (first pipes) (conj visited (first pipes)))))))

(def window-offsets (for [x (range -1 2) y (range -1 2)
                          :when (not (= (abs x) (abs y)))] 
                      [x y]))

(defn surrounding-pipes
  [location lines]
  (into #{} (map first 
                 (filter #(valid-connection? %1 location)
                         (map vector 
                              (surrounding-pipes-init location lines) 
                              [:up :left :right :down])))))

(defn surrounding-pipes-init
  [location lines]
  (for [offset window-offsets]
    (let [coords (map + (:coord location) offset)] 
      (->pipeinfo coords (get-in lines coords)))))

(defn valid-connection?
  [[to-pipe direction] current-pipe]
  (and (contains? (direction pipe-to-dir) (:pipe to-pipe))
       (contains? ((direction opposite) pipe-to-dir) (:pipe current-pipe))))


(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file2)
(solve2 real_input_file)

