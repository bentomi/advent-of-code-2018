(ns advent-of-code-2018.day17.code
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defonce line-pattern #"([xy])=(\d+), ([xy])=(\d+)\.\.(\d+)")

(defn parse-line [line]
  (let [[_ d0 pos d1 r0 r1] (re-matches line-pattern line)]
    (array-map (keyword d0) (Integer/parseInt pos)
               (keyword d1) [(Integer/parseInt r0) (Integer/parseInt r1)])))

(defonce input
  (with-open [rdr (io/reader "src/advent_of_code_2018/day17/input.txt")]
    (->> rdr line-seq (mapv parse-line))))

(def clay \#)
(def sand \.)
(def water \~)
(def wet \|)

(defn shift-left [shift {:keys [x y] :as line}]
  (assoc line :x (if (vector? x) (mapv #(- % shift) x) (- x shift))))

(let [xs (map #(let [x (:x %)] (if (vector? x) (second x) x)) input)
      ys (map #(let [y (:y %)] (if (vector? y) (second y) y)) input)
      min-x (dec (apply min xs))]
  (def normalized-input (mapv (partial shift-left min-x) input))
  (def spring [0 (- 500 min-x)])
  (def max-x (- (inc (apply max xs)) min-x))
  (def max-y (apply max ys)))

(defn set-clay [scan {:keys [x y]}]
  (if (vector? x)
    (reduce #(assoc-in %1 [y %2] clay) scan (range (first x) (inc (second x))))
    (reduce #(assoc-in %1 [%2 x] clay) scan (range (first y) (inc (second y))))))

(def scan
  (let [s (vec (repeat (inc max-y) (vec (repeat (inc max-x) sand))))
        s (reduce set-clay s normalized-input)]
    (assoc-in s spring \+)))

(defn draw-scan [scan]
  (doseq [r scan]
    (doseq [c r]
      (print c))
    (println)))

(defn up [[y x]] [(dec y) x])
(defn down [[y x]] [(inc y) x])
(defn left [[y x]] [y (dec x)])
(defn right [[y x]] [y (inc x)])

(defn below [[y] depth] (< depth y))

(defn blocked [scan pos]
  (#{clay water} (get-in scan pos)))

(declare spread)

(defn trickle [scan pos min-depth]
  (let [d (down pos)]
    (cond
      (>= (first d) (count scan)) [:drawn scan]
      (= wet (get-in scan d)) [:drawn scan]
      (= sand (get-in scan d)) (recur (assoc-in scan d wet) d min-depth)
      :else (let [[lres scan] (spread left scan pos wet)
                  [rres scan] (spread right scan pos wet)]
              (if (some #{:drawn} [lres rres])
                [:drawn scan]
                (let [[_ scan] (spread left scan pos water)
                      [_ scan] (spread right scan pos water)
                      scan (assoc-in scan pos water)]
                  (if (below pos min-depth)
                    (recur scan (up pos) min-depth)
                    [:filled scan])))))))

(defn spread [dir scan pos marker]
  (let [n (dir pos)]
    (cond
      (blocked scan n) [:filled scan]
      (every? #{wet} [marker (get-in scan n)]) [:drawn scan]
      :else (let [scan (assoc-in scan n marker)
                  nd (down n)]
              (if (blocked scan nd)
                (recur dir scan n marker)
                (let [[res scan] (trickle scan n (inc (first n)))]
                  (if (= res :drawn)
                    [res scan]
                    (recur dir scan n marker))))))))

(defn count-water [scan water-type]
  (->> scan
       (drop-while #(every? (complement #{clay}) %))
       (apply concat) (filter water-type) count))

(deftest small-test
  (let [scan (mapv vec ["......+......."
                        "............#."
                        ".#..#.......#."
                        ".#..#..#......"
                        ".#..#..#......"
                        ".#.....#......"
                        ".#.....#......"
                        ".#######......"
                        ".............."
                        ".............."
                        "....#.....#..."
                        "....#.....#..."
                        "....#.....#..."
                        "....#######..."])
        [res scan] (trickle scan [0 6] 0)]
    (is (= :drawn res))
    (is (= 57 (count-water scan #{wet water})))
    (is (= 29 (count-water scan #{water})))))

(let [[res scan] (trickle scan spring 0)]
  (deftest problem1-test
    (is (= :drawn res))
    (is (= 39162 (count-water scan #{wet water}))))
  (deftest problem2-test
    (is (= 32047 (count-water scan #{water})))))
