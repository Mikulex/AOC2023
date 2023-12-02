(ns day01)
(require '[clojure.string :as str])
(use 'clojure.java.io)
(def demo_input_file (str (System/getProperty "user.dir") "/inputs/day01/" "demo.txt"))
(def demo2_input_file (str (System/getProperty "user.dir") "/inputs/day01/" "demo2.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/inputs/day01/" "input.txt"))
(def digit-map {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"})

(defn parse-digit-literal 
  "Splits String s and returns digit Strings"
  [s]
  (filter #(re-find #"\d" %) (str/split s #"")))

(defn normalize-digits 
  "Converts words to digit literals and returns literals as is"
  [potential-digits] 
  (map 
    #(if (re-find #"\d" %) 
      %
      (digit-map %)) 
    potential-digits))

(defn parse-digit-spelled 
  "Parses digit literals and words alike by some kind of sliding window strategy. Converts words to digit Strings"
  [s]
  (normalize-digits 
    (loop [nums []
           index 0]
      (let [res (re-find #"(\d|one|two|three|four|five|six|seven|eight|nine)" (subs s index))]
        (if (nil? res)
          nums
          (recur 
            (conj nums (last res)) 
            (max 
              (inc index)
              (+ (dec (count (last res))) (str/index-of s (last res) index)))))))))

(defn parse-numbers 
  "Collects digits based on parsing strategy f and retuns the joined first and last ones as an Integer"
  [f s]
  (let [digits (f s)]
      (Integer. (str/join (concat (first digits) (last digits))))))

(defn calculate 
  "Sums up parsed numbers. based on parsing strategy f"
  [f lines]
  (reduce + (map #(parse-numbers f %) lines)))

(defn solve1 
  [file]
  (with-open [rdr (reader file)] 
    (calculate parse-digit-literal (doall (line-seq rdr)))))

(defn solve2 
  [file]
  (with-open [rdr (reader file)] 
    (calculate parse-digit-spelled (doall (line-seq rdr)))))

(solve1 demo_input_file)
(solve1 real_input_file)

(solve2 demo2_input_file)
(solve2 real_input_file)


