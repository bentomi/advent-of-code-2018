(ns advent-of-code-2018.day11.code
  (:require [clojure.test :refer [is]]))

(def input 7672)

(defn power-level [grid-id x y]
  (let [rack-id (+ x 10)]
    (-> rack-id
        (* y)
        (+ grid-id)
        (* rack-id)
        (quot 100)
        (mod 10)
        (- 5))))

(is (= 4 (power-level 8 3 5)))
(is (= -5 (power-level 57 122 79)))
(is (= 0 (power-level 39 217 196)))
(is (= 4 (power-level 71 101 153)))

(defn power-grid [grid-id]
  (vec (for [y (range 1 301)]
         (vec (for [x (range 1 301)]
                (power-level grid-id x y))))))

(defonce grid
  (power-grid input))

(defn field-at [grid x y]
  (mapv #(subvec % (dec x) (+ x 2)) (subvec grid (dec y) (+ y 2))))

(defn power-at [grid x y]
  (reduce + (map #(reduce + %) (field-at grid x y))))

(defn problem1 []
  (second
   (apply max-key first
          (for [x (range 1 299)
                y (range 1 299)]
            [(power-at grid x y) [x y]]))))

(is (= [22,18] (problem1)))
