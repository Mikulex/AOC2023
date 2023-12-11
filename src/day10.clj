(ns day10
  (:require clojure.repl) 
  (:require
    [clojure.string :as str]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day10/" "demo.txt"))
(def demo_input_file2 (str (System/getProperty "user.dir") "/inputs/day10/" "demo2.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day10/" "input.txt"))
(declare surrounding-pipes
         surrounding-pipes-init 
         valid-connection? 
         pipe-loop 
         shoelace)

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

(defn find-start [lines]
  (as-> (map-indexed #(vector %1 (str/index-of %2 "S")) lines) l
    (filter #(number? (second %)) l )
    (flatten l)
    (->pipeinfo l \S)))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")
        loop-start (find-start lines)]
    (/ (count (pipe-loop loop-start lines)) 2)))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")
        loop-start (find-start lines)
        pipes (pipe-loop loop-start lines)]
    (- (shoelace pipes) (dec (/ (count pipes) 2)))))

(defn shoelace
  "Calculates the area based on Gauss' Shoelace formula. Will also include area of the pipes themselves"
  [pipes]
  (abs (/ (loop [remaining (conj (mapv :coord pipes) (:coord (first pipes)))
                 sum 0]
            (if (<= (count remaining) 1) sum
              (let [[fx fy] (first remaining)
                    [sx sy] (second remaining)]
                (recur (rest remaining) (+ sum (- (* fx sy) (* fy sx)))))))
          2)))

(defn filter-pipes
  [visited pipes]
  (filter (fn [found-pipe] (not (some #(= found-pipe %) visited)))
          pipes))

(defn pipe-loop
  [loop-start 
   lines]
  (loop [pipe loop-start
         visited (vector loop-start)]
    (let [pipes (filter-pipes visited (surrounding-pipes pipe lines))]
      (if (empty? pipes)
        visited
        (recur (first pipes) (conj visited (first pipes)))))))

(def window-offsets (for [x (range -1 2) y (range -1 2)
                          :when (not (= (abs x) (abs y)))] 
                      [x y]))

(defn surrounding-pipes
  "Collects surrounding pipes and only returns those that would are actually connected"
  [location lines]
  (into #{} (map first 
                 (filter (fn [direction] (valid-connection? direction location))
                         (map vector 
                              (surrounding-pipes-init location lines) 
                              [:up :left :right :down])))))

(defn surrounding-pipes-init
  "Collects the raw surrounding pipes"
  [location lines]
  (for [offset window-offsets]
    (let [coords (map + (:coord location) offset)] 
      (->pipeinfo coords (get-in lines coords)))))

(defn valid-connection?
  "Checks if there is an entry for the pipe we walk to and an exit for the pipe we are currently in"
  [[to-pipe direction] current-pipe]
  (and (contains? (direction pipe-to-dir) (:pipe to-pipe))
       (contains? ((direction opposite) pipe-to-dir) (:pipe current-pipe))))


(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file2)
(solve2 real_input_file)

