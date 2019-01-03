(ns advent-of-code-2018.day18.code
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [medley.core :as m]))

(defonce input
  (with-open [rdr (io/reader "src/advent_of_code_2018/day18/input.txt")]
    (->> rdr line-seq (mapv vec))))

(def open-ground \.)
(def trees \|)
(def lumberyard \#)

(defn count-type [type-pred fields]
  (->> fields (filter type-pred) count))

(defn change [field neighbours]
  (condp = field
    open-ground (if (< 2 (count-type #{trees} neighbours)) trees open-ground)
    trees (if (< 2 (count-type #{lumberyard} neighbours)) lumberyard trees)
    lumberyard (if (and (pos? (count-type #{lumberyard} neighbours))
                        (pos? (count-type #{trees} neighbours)))
                 lumberyard
                 open-ground)))

(defn neighbour-pos [[y x]]
  [[(dec y) (dec x)] [(dec y) x] [(dec y) (inc x)]
   [y (dec x)]                   [y (inc x)]
   [(inc y) (dec x)] [(inc y) x] [(inc y) (inc x)]])

(defn neighbour-fields [scan pos]
  (map #(get-in scan %) (neighbour-pos pos)))

(defn change-at [scan pos]
  (change (get-in scan pos) (neighbour-fields scan pos)))

(defn step [scan]
  (reduce #(assoc-in %1 %2 (change-at scan %2))
          scan
          (for [[y r] (m/indexed scan), x (range (count r))] [y x])))

(defn resource-value [scan]
  (let [fields (apply concat scan)
        wooded (count-type #{trees} fields)
        lumberyards (count-type #{lumberyard} fields)]
    (* wooded lumberyards)))

(deftest problem1-test
  (is (= 737800 (resource-value (nth (iterate step input) 10)))))

;; Starting from minute 432 the resource values cycle through.
(def cycle-offset 432)
(def cycle-values
  [186063 191070 199332 204336 210396 211344 214328 210864 212040 202768
   196014 187758 187278 174264 176256 175066 176673 174945 176880 171802
   170625 168960 172250 170984 174636 176134 179725 179597])

(deftest problem2-test
  (is (= 212040
         (cycle-values (mod (- 1000000000 cycle-offset)
                            (count cycle-values))))))
