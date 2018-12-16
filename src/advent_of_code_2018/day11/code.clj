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
  (mapv #(subvec % x (+ x 3)) (subvec grid y (+ y 3))))

(defn power-at [grid x y]
  (reduce + (map #(reduce + %) (field-at grid x y))))

(defn problem1 []
  (second
   (apply max-key first
          (for [x (range 0 298)
                y (range 0 298)]
            [(power-at grid x y) [(inc x) (inc y)]]))))

(is (= [22 18] (problem1)))

(defn max-power-at
  ([grid x y]
   (let [max-size (min (- (-> grid first count) x) (- (count grid) y))]
     (max-power-at grid x y max-size)))
  ([grid x y max-size]
   (let [pxy (get-in grid [y x])]
     (loop [size 1
            power pxy
            [mpower :as max-power] [pxy 1]]
       (if (>= size max-size)
         max-power
         (let [size' (inc size)
               new-col (map #(get-in grid [% (+ x size)])
                            (range y (+ y size')))
               new-row (subvec (grid (+ y size)) x (+ x size'))
               power' (reduce + power (concat new-col new-row))]
           (recur size'
                  power'
                  (if (> power' mpower) [power' size'] max-power))))))))

(defn problem2 []
  (apply max-key #(get-in % [2 0])
         (for [y (range 0 (count grid))
               x (range 0 (-> grid first count))]
           [(inc x) (inc y) (max-power-at grid x y)])))

(is (= [234 197 [114 14]] (problem2)))
