(ns advent-of-code-2018.day12.code
  (:require [clojure.test :refer [deftest is]]))

(def initial-state
  "##..##..#.##.###....###.###.#.#.######.#.#.#.#.##.###.####..#.###...#######.####.##...#######.##..#")

(def char-map {\. 0, \# 1})

(def producers
  (set (map #(map char-map %)
            ["...##" "..#.#" ".#..." ".#..#" ".#.##" ".##.." ".###." ".####"
             "#...#" "#..#." "#.#.." "#.#.#" "##.#." "###.#" "#####"])))

(defn normalise-generation [{:keys [start state]}]
  (let [start-zeros (count (take-while zero? state))
        diff (- 3 start-zeros)]
    {:start (- start diff)
     :state (if (pos? diff)
              (into state (repeat diff 0))
              (drop (- diff) state))}))

(defn make-generation [state-string]
  (normalise-generation {:start 0 :state (map char-map state-string)}))

(defn step [{:keys [state] :as generation}]
  (normalise-generation
   (assoc generation
          :state (list* 0 0 (map #(if (producers %) 1 0)
                                 (partition 5 1 (concat state [0 0 0])))))))

(defn steps [n f state]
  (if (pos? n)
    (recur (dec n) f (f state))
    state))

(defn sum-plants [{:keys [start state]}]
  (reduce + (map * (range start (+ start (count state))) state)))

(deftest problem1
  (is (= 1816 (sum-plants (steps 20 step (make-generation initial-state))))))

(deftest problem2
  (is (= 3957 (sum-plants (steps 500 step (make-generation initial-state)))))
  (is (= 39957 (sum-plants (steps 5000 step (make-generation initial-state)))))
  (is (= 399957 (sum-plants (steps 50000 step (make-generation initial-state))))))
