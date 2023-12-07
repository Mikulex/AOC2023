(ns day07
  (:use clojure.repl))
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day07/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day07/" "input.txt"))
(defrecord hand-info [rank hand cards bid])
(def values {:high 0 :pair 1 :two-pairs 2 :triple 3 :full-house 4 :quad 5 :quint 6})
(def card-scores (into {} (map vector "23456789TJQKA" (range 13))))
(def card-scores-joker (into {} (map vector "J23456789TQKA" (range 13))))

(defn parse-hands 
  [lines]  
  (map #(->hand-info 0 nil (first %) (parse-long (last %))) (map #(str/split % #" ") lines)))

(defn compare-cards
  [f s mappings]
  (let [fscore (get mappings (first f))
        sscore (get mappings (first s))]
    (cond
      (not (= fscore sscore)) (- fscore sscore)
      (nil? fscore) 0
      :else (recur (str/join (rest f)) (str/join (rest s)) mappings))))

(defn eval-hand
  [card-group]
  (let [counts (map last card-group)]
    (cond
      (= (first counts) 5) :quint
      (= (first counts) 4) :quad
      (= (first counts) 3) (if (= (second counts) 2) :full-house :triple)
      (= (first counts) 2) (if (= (second counts) 2) :two-pairs :pair)
      :else :high)))

(defn card-groups 
  "Splits the card string to a sorted list of maps of type->count"
  [cards]
  (sort #(> (last %1) (last %2))
        (for [[card amount] (group-by identity (sort cards))]
          [card (count amount)])))

(defn card-groups-converted-jokers
  "Splits the card string to a sorted list of maps of type->count and applies jokers to the type with the highest count"
  [cards]
  (let [mappings (into {} (for [[card amount] (group-by identity (sort cards))] [card (count amount)]))]
    (let [jokers (get mappings \J)
          updates (into [] (sort #(> (last %1) (last %2)) (dissoc mappings \J)))]
      (cond 
        (nil? jokers) updates
        (= jokers 5) [[\J 5]]
        :else (update updates 0 (fn [old-mapping] (update old-mapping 1 #(+ jokers %))))))))

(defn sorted-hands
  [lines scores grouping-func]
  (sort-by #(get values (:hand %1))
           (sort #(compare-cards (:cards %1) (:cards %2) scores) 
                 (map 
                   (fn [hand] (assoc hand :hand (eval-hand (grouping-func (:cards hand)))))
                   (parse-hands lines)))))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    (reduce + (map #(* (:rank %) (:bid %)) 
         (map-indexed #(assoc %2 :rank (inc %1)) 
                      (sorted-hands lines card-scores card-groups))))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]
    (reduce + (map #(* (:rank %) (:bid %)) 
         (map-indexed #(assoc %2 :rank (inc %1)) 
                      (sorted-hands lines card-scores-joker card-groups-converted-jokers))))))


(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
