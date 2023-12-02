(ns day02)
(require '[clojure.string :as str])
(use 'clojure.java.io)
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day02/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day02/" "input.txt"))
(defrecord game [id subsets])
(def max-cubes {"red" 12 "green" 13 "blue" 14})

(defn parse-game
  [s]
  (let [[id & r] (str/split s #": ")]
    (map->game 
      {:id (Integer. id)
       :subsets (->> (str/split (first r) #"; ")
                     (map #(str/split % #", "))
                     (map (fn [subset]
                            (->> subset
                                 (map #(str/split % #" "))
                                 (map rseq)
                                 (map #(vector (first %) (Integer. (last %))))
                                 (into {})))))})))

(defn valid-game?
  [game]
  (every? true?
          (map (fn [subset]
                 (->> subset
                      (map (fn [[color value]] (>= (get max-cubes color) value)))
                      (every? true?)))
               (:subsets game))))

(defn solve1 
  [file]
  (with-open [rdr (reader file)] 
    (as-> (doall (line-seq rdr)) v
      (map parse-game v)
      (group-by valid-game? v)
      (get v true)
      (map #(:id %) v)
      (reduce + v)
      )))

(defn solve2
  [file]
  (with-open [rdr (reader file)] 
    (->> (doall (line-seq rdr))
      (map parse-game)
      (map :subsets)
      (map (fn [subset] (reduce #(merge-with max %1 %2) {} subset) ))
      (map vals) 
      (map #(apply * %))
      (apply +)
      )))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)

