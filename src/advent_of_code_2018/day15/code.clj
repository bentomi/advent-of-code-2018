(ns advent-of-code-2018.day15.code
  (:require [clojure.test :refer [deftest is]]))

(defonce input-file "src/advent_of_code_2018/day15/input.txt")

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> rdr line-seq (mapv vec))))

(def free \.)

(def races #{\G \E})

(def other-race {\E \G, \G \E})

(defn warriors [input]
  (into (sorted-map)
        (for [y (range 0 (count input))
              :let [row (get input y)]
              x (range 0 (count row))
              :let [race (races (get row x))]
              :when race]
          [[y x] {:race race, :hp 200}])))

(defn find-targets [{:keys [race]} {:keys [warriors]}]
  (map first (filter #(-> % second :race #{(other-race race)}) warriors)))

(defn adjacent [[y x]]
  [[(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]])

(defn free-adjacent [field pos]
  (filter #(= free (get-in field %)) (adjacent pos)))

(defn bfs [field start-pos targets]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start-pos []])
         visited #{}
         solutions []]
    (when-let [[pos path] (peek q)]
      (if (or (empty? solutions) (< (count path) (count (first solutions))))
        (let [path' (conj path pos)]
          (if (targets pos)
            (recur (pop q)
                   (conj visited pos)
                   (conj solutions path'))
            (let [nposs (filter (complement visited) (free-adjacent field pos))]
              (recur (into (pop q) (map (fn [pos] [pos path']) nposs))
                     (conj visited pos)
                     solutions))))
        solutions))))

(defn warrior-move [{:keys [field warriors]} pos]
  (let [targets (find-targets (get-in field pos) warriors)
        ;; test if a target is already in range, else move
        in-range (set (mapcat #(free-adjacent field %) targets))]
    (first (sort-by (juxt peek first) (bfs field start-pos in-range)))))
