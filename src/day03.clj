(ns day03)
(require '[clojure.string :as str])
(use 'clojure.java.io)
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day03/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day03/" "input.txt"))

(defn re-seq-pos [pattern string] 
  (let [m (re-matcher pattern string)] 
    ((fn step [] 
      (when (. m find) 
        (cons (. m start) (lazy-seq (step))))))))

(defn window-ranges 
  [row idx]
  [(range (dec row) (+ row 2)) (range (dec idx) (+ idx 2))])

(defn find-preceding-digits [line idx] 
  (str/reverse (re-find  #"\d+" (str/reverse (subs line 0 idx)))))

(defn find-next-digits [line idx] 
  (re-find  #"\d+" (subs line (inc idx))))

(defn kernel [row idx lines]
  (let [ranges (window-ranges row idx)]
    (for [current-row (first ranges)]
      (for [current-idx (last ranges)]
        (let [current-char (get-in lines [current-row current-idx])]
          (if (Character/isDigit current-char)
            (cond
              (= current-idx (first (last ranges))) (find-preceding-digits (get lines current-row) idx)
              (= current-idx (last (last ranges))) (find-next-digits (get lines current-row) idx)
              :else (str current-char))
            "."
            ))))))

(defn part-numbers 
  [row idx lines]
  (filter not-empty (mapcat #(str/split % #"\.") (map str/join (kernel row idx lines)))))

(defn solve1 
  [file]
  (let [lines (str/split (slurp file) #"\n")
        machine-parts (vec (map #(re-seq-pos #"[@#$%&*+\-\/=]" %) lines))]
    (reduce + (flatten
                (for [row (range (count lines)) 
                      :when (seq? (get machine-parts row))]
                  (map parse-long (mapcat #(part-numbers row % lines) (get machine-parts row))))))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo_input_file)
(solve2 real_input_file)

  
