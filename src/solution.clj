(ns solution)
(require '[clojure.string :as str])
(use 'clojure.java.io)
(def demo_input_file (str (System/getProperty "user.dir") "/" "demo.txt"))
(def real_input_file (str (System/getProperty "user.dir") "/" "input.txt"))

(defn parse-digits [s]
  (let [digits (filter #(re-find #"\d" %) (str/split s #""))]
    (Integer. (str/join (concat (first digits) (last digits))))))

(defn calculate [lines]
  (reduce + (map parse_digits lines)))

(defn solve1 [file]
  (with-open [rdr (reader file)] 
    (calculate (doall (line-seq rdr)))))

(solve1 demo_input_file)
(solve1 real_input_file)

