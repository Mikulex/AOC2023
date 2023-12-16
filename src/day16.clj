(ns day16
  (:require clojure.repl
            [clojure.set :as set]) 
  (:require [clojure.string :as str]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day16/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day16/" "input.txt"))

(def direction {:north [-1 0] :south [1 0] :west [0 -1] :east [0 1]})

(def turns {:north {:left :west, :right :east}
            :south {:left :east, :right :west}
            :east {:left :north, :right :south}
            :west {:left :south, :right :north}})

(defn reflect [tile dir]
  (when-not (some #(= tile %) [\\ \/]) (throw (Exception. (str "Invalid tile [" tile "]"))))
  (cond
    (some #(= dir %) [:north :south]) (if (= tile \\) (get-in turns [dir :left]) (get-in turns [dir :right]))
    (some #(= dir %) [:east :west]) (if (= tile \\) (get-in turns [dir :right]) (get-in turns [dir :left]))
    :else (throw (Exception. "unmatched case while reflecting"))))

(defn splits? [tile dir]
  (when-not (some #(= tile %) [\- \|]) (throw (Exception. (str "Invalid tile [" tile "]"))))
  (cond
    (some #(= dir %) [:north :south]) (= tile \-)
    (some #(= dir %) [:east :west]) (= tile \|)
    :else (throw (Exception. "unmatched case while splitting"))))

(defn energized-tiles [lines]
  (letfn [(iter [cur facing visited]
            (let [cur-tile (get-in lines cur)
                  offset (facing direction)
                  next-coord-same-dir (map + cur offset)]
              (cond
                (nil? cur-tile) visited
                (contains? visited [cur facing]) visited
                (= cur-tile \.) (recur next-coord-same-dir facing (conj visited [cur facing]))
                (some #(= cur-tile %) [\- \|]) (if (splits? cur-tile facing)
                                                 (apply set/union (map #(iter (map + cur (% direction)) % (conj visited [cur facing]))
                                                                      (vals (facing turns))))
                                                 (recur next-coord-same-dir facing (conj visited [cur facing])))
                (some #(= cur-tile %) [\\ \/]) (recur (map + cur ((reflect cur-tile facing) direction)) (reflect cur-tile facing) (conj visited [cur facing]))
                :else (throw (Exception. "unmatched case while traversing")))))]
    (iter [0 0] :east #{})))


(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    (count (into #{} (map first (energized-tiles lines))))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    ))

(solve1 demo_input_file)
(solve1 real_input_file)
;(solve2 demo_input_file)
;(solve2 real_input_file)
