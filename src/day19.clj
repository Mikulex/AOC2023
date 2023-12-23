(ns day19
  (:require clojure.repl) 
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day19/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day19/" "input.txt"))
(defrecord part [x m a s current-res])
(defrecord rule [label rules])
(defrecord expr [expr variable target])

(defn parse-conds [expr-string]
  (let [conds (map last (re-seq #"(\w[<>]\d*:\w+|\w+)(?=[,}])" expr-string))]
    (for [expr conds]
      (let [[test-expr target] (str/split expr #":")]
        (cond
          (nil? target)
            (->expr nil nil test-expr)
          (some? (str/index-of test-expr ">"))
          (let [[k v] (str/split test-expr #">")]
            (->expr (str "(> " (keyword k) " " v ")") (keyword k) target))
          :else
          (let [[k v] (str/split test-expr #"<")]
            (->expr (str "(< " (keyword k) " " v ")") (keyword k) target)))))))

(defn parse-rules [rules]
  (->> rules
       (map #(str/split % #"\{")) ;}
       (map #(hash-map (first %) (parse-conds (last %))))
       (into {})))
       
(defn parse-parts [parts]
  (->> parts
       (mapv #(re-seq #"\d+" %))
       (map vec)
       (map #(conj % "in"))
       (map #(apply ->part %))))

(defn insert-var [expr part]
  (let [variable (:variable expr)]
    (str/replace (:expr expr) 
                 (str variable) 
                 (get part variable))))

(defn apply-rule [part rule-obj]
  (let [rules (:rules rule-obj)]
  (loop [remaining rule-obj]
    (let [cur-expr (first remaining)]
    (cond 
      (nil? (:expr cur-expr)) (assoc part :current-res (:target cur-expr))
      (eval (read-string (insert-var cur-expr part))) (assoc part :current-res (:target cur-expr))
      :else (recur (rest remaining)))))))

(defn apply-workflow [part rules-map]
 (if (or (= (:current-res part) "A") (= (:current-res part) "R"))
   part
    (recur (apply-rule part (get rules-map (:current-res part))) rules-map)))

(defn solve1
  [file]
  (let [[rules parts] (str/split (slurp file) #"\n\n")
        rules-map (parse-rules (str/split rules #"\n"))
        parsed-parts (parse-parts (str/split parts #"\n"))]
    (->> parsed-parts
         (map #(apply-workflow % rules-map))
         (filter #(= (:current-res %) "A"))
         (map vals)
         (flatten)
         (map parse-long)
         (filter some?)
         (reduce +))))

(solve1 demo_input_file)
(solve1 real_input_file)
