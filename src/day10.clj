(ns day10
  (:require clojure.repl) 
  (:require
    [clojure.string :as str]))
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day10/" "demo.txt"))
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

