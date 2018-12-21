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

(defn find-targets [{:keys [race]} warriors]
  (map first (filter #(-> % second :race #{(other-race race)}) warriors)))

(defn adjacent [[y x]]
  [[(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]])

(defn match-adjacent [pred field pos]
  (filter #(pred (get-in field %)) (adjacent pos)))

(defn free-adjacent [field pos]
  (match-adjacent #{free} field pos))

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
  (let [targets (find-targets (get warriors pos) warriors)]
    (if (seq targets)
      (let [field-without (assoc-in field pos \.)
            in-range (set (mapcat #(free-adjacent field-without %) targets))]
        (if (seq (filter #{pos} in-range))
          :in-range
          (if (seq in-range)
            (or (->> (bfs field pos in-range)
                     (sort-by (juxt peek identity))
                     first
                     second)
                :no-path)
            :no-reachable-target)))
      :no-target)))

(def attack-power 3)

(defn add-warrior [state pos warrior]
  (-> state
      (assoc-in (cons :field pos) (:race warrior))
      (update :warrior assoc pos warrior)))

(defn remove-warrior [state pos]
  (-> state
      (assoc-in (cons :field pos) free)
      (update :warrior dissoc pos)))

(defn pick-enemy [{:keys [field warriors]} pos]
  (let [enemy-race (other-race (get-in warriors [pos :race]))]
    (->> (match-adjacent #{enemy-race} field pos)
         (sort-by (juxt #(get-in warriors [% :hp]) identity))
         first)))

(defn attack [{:keys [field warriors] :as state} pos]
  (if-let [enemy-pos (pick-enemy state pos)]
    (if (<= (get-in warriors [enemy-pos :hp]) attack-power)
      (remove-warrior state enemy-pos)
      (update-in state [:warriors enemy-pos :hp] - attack-power))
    state))

(defn warrior-step [state from to]
  (-> state
      (remove-warrior from)
      (add-warrior to (get-in state [:warriors from]))))

(defn turn [{:keys [field warriors] :as state} pos]
  (let [move (warrior-move state pos)]
    (cond
      (vector? move) (attack (warrior-step state pos move) move)
      (= :in-range move) (attack state pos)
      (= :no-target move) :battle-end
      :else state)))
