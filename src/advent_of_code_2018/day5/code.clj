(ns advent-of-code-2018.day5.code
  (:require [clojure.string :as str]))

(defonce input-file "src/advent_of_code_2018/day5/input.txt")

(defonce input (str/trim-newline (slurp input-file)))

(defn react [polymer]
  (let [b (StringBuilder. polymer)]
    (loop [i 0]
      (if (< i (dec (count b)))
        (if (= 32 (Math/abs (- (.codePointAt b i) (.codePointAt b (inc i)))))
          (do (.delete b i (+ i 2))
              (recur (max (dec i) 0)))
          (recur (inc i)))
        (str b)))))

(defonce problem1 (count (react input)))

(defonce problem2
  (->> (for [i (range (int \a) (inc (int \z)))
             :let [c (char i)
                   s (str/replace input (str c) "")
                   s (str/replace s (str (char (- i 32))) "")]]
         [c (count (react s))])
       (apply min-key second)
       last))
