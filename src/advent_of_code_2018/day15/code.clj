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

(defn find-targets [race warriors]
  (map first (filter #(-> % second :race #{(other-race race)}) warriors)))

(defn adjacent [[y x]]
  [[(dec y) x] [y (dec x)] [y (inc x)] [(inc y) x]])

(defn match-adjacent [pred field pos]
  (filter #(pred (get-in field %)) (adjacent pos)))

(defn free-adjacent [field pos]
  (match-adjacent #{free} field pos))

(defn bfs [field start-pos targets]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY [start-pos []])
         seen #{start-pos}
         solutions []]
    (let [[pos path] (peek q)]
      (if (and pos
               (or (empty? solutions)
                   (<= (count path) (count (first solutions)))))
        (let [path' (conj path pos)]
          (if (targets pos)
            (recur (pop q)
                   seen
                   (conj solutions (subvec path' 1)))
            (let [nposs (remove seen (free-adjacent field pos))]
              (recur (into (pop q) (map (fn [pos] [pos path']) nposs))
                     (into seen nposs)
                     solutions))))
        solutions))))

(defn warrior-move [{:keys [field warriors]} pos]
  (let [targets (find-targets (get-in field pos) warriors)]
    (if (seq targets)
      (let [field-without (assoc-in field pos \.)
            in-range (set (mapcat #(free-adjacent field-without %) targets))]
        (if (in-range pos)
          :in-range
          (if (seq in-range)
            (or (ffirst (sort-by (juxt peek first) (bfs field pos in-range)))
                :no-path)
            :no-reachable-target)))
      :no-target)))

(defn add-warrior [state pos warrior]
  (-> state
      (assoc-in (cons :field pos) (:race warrior))
      (update :warriors assoc pos warrior)))

(defn remove-warrior [state pos]
  (-> state
      (assoc-in (cons :field pos) free)
      (update :warriors dissoc pos)))

(defn pick-enemy [{:keys [field warriors]} pos]
  (let [enemy-race (other-race (get-in warriors [pos :race]))]
    (->> (match-adjacent #{enemy-race} field pos)
         (sort-by (juxt #(get-in warriors [% :hp]) identity))
         first)))

(defn attack [{:keys [field warriors attack-powers] :as state} pos]
  (let [attack-power (attack-powers (get-in warriors [pos :race]))]
    (if-let [enemy-pos (pick-enemy state pos)]
      (if (<= (get-in warriors [enemy-pos :hp]) attack-power)
        (remove-warrior state enemy-pos)
        (update-in state [:warriors enemy-pos :hp] - attack-power))
      state)))

(defn warrior-step [state from to]
  (-> state
      (remove-warrior from)
      (add-warrior to (get-in state [:warriors from]))))

(defn turn [{:keys [field warriors] :as state} pos]
  (let [move (if (contains? warriors pos) (warrior-move state pos) :skip)]
    (cond
      (vector? move) (attack (warrior-step state pos move) move)
      (= :in-range move) (attack state pos)
      (= :no-target move) :battle-end
      :else state)))

(defn round [{:keys [warriors] :as state}]
  (loop [state state
         poss (keys warriors)]
    (if-let [[pos & poss'] (seq poss)]
      (let [turn-result (turn state pos)]
        (if (= :battle-end turn-result)
          [:battle-end state]
          (recur turn-result poss')))
      [:round-end state])))

(defn draw-warriors-in-row [warriors y]
  (doseq [[[r] {:keys [race hp]}] warriors
          :when (= r y)]
    (printf " %s(%d)" race hp)))

(defn draw-field [{:keys [field warriors]}]
  (doseq [[y row] (map vector (range) field)]
    (doseq [cell row]
      (print cell))
    (draw-warriors-in-row warriors y)
    (println)))

(defn battle
  ([state] (battle state nil))
  ([state options]
   (loop [state state
          full-rounds 0]
     (when (:trace options)
       (println "After" full-rounds "full rounds:")
       (draw-field state))
     (let [[round-result state'] (round state)]
       (if (= :battle-end round-result)
         [state' full-rounds]
         (recur state' (inc full-rounds)))))))

(defn battle-outcome
  ([battle-result] (battle-outcome battle-result nil))
  ([[state' full-rounds] options]
   (when (:trace options)
     (println "After" full-rounds "full rounds and end of battle:")
     (draw-field state'))
   (let [hps (reduce #(+ %1 (get-in %2 [1 :hp])) 0 (:warriors state'))]
     (* full-rounds hps))))

(defn warrior-count [race-pred state]
  (->> (:warriors state) (filter #(-> % second :race race-pred)) count))

(defn battle-outcome-without-elf-death
  ([state] (battle-outcome-without-elf-death state nil))
  ([state options]
   (let [[state' :as battle-result] (battle state options)]
     (when (= (warrior-count #{\E} state) (warrior-count #{\E} state'))
       (battle-outcome battle-result options)))))

(def initial-state
  {:field input :warriors (warriors input)
   :attack-powers {\E 3, \G 3}})

(deftest problem1
  (is (= 227290 (battle-outcome (battle initial-state)))))

(deftest problem2
  (is (nil? (battle-outcome-without-elf-death
             (assoc-in initial-state [:attack-powers \E] 24) {:trace false})))
  (is (= 53725 (battle-outcome-without-elf-death
                (assoc-in initial-state [:attack-powers \E] 25) {:trace false}))))
