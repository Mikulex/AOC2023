(ns day12
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day12/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day12/" "input.txt"))

(defn parse-lines
  [lines]
  (->> (map #(filter not-empty (str/split % #"[\s.]")) lines)
       (map #(split-at (dec (count %)) %))
       (map #(vector (str/join "." (first %)) (map parse-long (str/split (first (second %)) #","))))))

(defn replace-first [s c] ( str/replace-first s #"." (str c)))

(defn line-solve
  [pattern number running]
  (loop [p pattern 
         n number
         r running]
    ;(prn p n r)
    (let [freqs (frequencies p)]
      (cond 
        (and (empty? p) (empty? n)) 1
        (and (empty? p) (= (reduce + n) 0)) 1
        (and (= (count n) 1) (= (get freqs \#) (first n))) 0
        (and (= (count n) 1) (= (first n) (count p)) (nil? (str/index-of p "."))) 1
        (and (= (count n) 1) (= (first n) (count p)) (not (nil? (str/index-of p ".")))) 1
        (and (empty? p) (> (reduce + n) 0))
        ;(do (prn \a)0) ; invalid because we have numbers left but no pattern
        (and (not (nil? (str/index-of p "#"))) (empty? n)) 
        (do (prn \b) 0) ; invalid because we still have springs but no numbers counting them
        (and (not (nil? (str/index-of p "#"))) (= (reduce + n) 0)) 
        (do (prn \c)0) ; invalid because we still have springs but no numbers counting them
        (and (= (first p) \#) (= (first n) 0))
        (do (prn \d) 0) ; invalid because we used up all springs and are still seeing some
        (and (= (first p) \.) (> (first n) 0) (= r true)) 
        (do (prn \e) 0) ; invalid because we expect to count springs but already found a dot
        (and (= (first p) \.) (= (first n) 0)) (recur (subs p 1 (count p)) (rest n) false)
        (= (first p) \#) (recur (subs p 1 (count p)) (conj (rest n) (dec (first n))) true)
        (= (first p) \.) (recur (subs p 1 (count p)) n false)
        (= (first p) \?) (+ (line-solve (replace-first p \#) n true) (line-solve (replace-first p \.) n false))
        :else (prn "unmatched case!" p n)))))

(line-solve "??#??.?#?" '(3 1) false)

(defn solve1 
  [file]
  (let [lines (parse-lines (str/split (slurp file) #"\n"))]
    (reduce + (map #(vector % (line-solve (first %1) (second %1) (= (first %) \#))) lines))))

(defn solve2
  [file]
  (let [lines (str/split (slurp file) #"\n")]))
;72751 too high
(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)
