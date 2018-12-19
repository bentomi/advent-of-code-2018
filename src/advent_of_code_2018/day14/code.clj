(ns advent-of-code-2018.day14.code
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def input 147061)

(def start-state {:board [3 7], :current [0 1]})

(defn round [{:keys [board current] :as state}]
  (let [scores (mapv board current)
        sum-scores (reduce + scores)
        new-recipes (map #(- (int %) (int \0)) (str sum-scores))
        board' (into board new-recipes)
        current' (mapv #(mod (+ %1 %2 1) (count board')) current scores)]
    (assoc state :board board' :current current')))

(defn recipes [n {:keys [board] :as state}]
  (if (< n (count board))
    state
    (recur n (round state))))

(defn target-scores [trainings recipe-count init-state]
  (let [state (recipes (+ trainings recipe-count) init-state)]
    (->> (:board state) (drop trainings) (take recipe-count))))

(defn score-string [scores]
  (str/join (map str scores)))

(deftest scores-test
  (is (= "5158916779" (score-string (target-scores 9 10 start-state))))
  (is (= "0124515891" (score-string (target-scores 5 10 start-state))))
  (is (= "9251071085" (score-string (target-scores 18 10 start-state))))
  (is (= "5941429882" (score-string (target-scores 2018 10 start-state)))))

(score-string (target-scores 147061 10 start-state))

(deftest problem1
  (is (= "2145581131" (score-string (target-scores 147061 10 start-state)))))

(defn ends-with [v sv]
  (let [d (- (count v) (count sv))]
    (and (nat-int? d)
         (= (subvec v d (count v)) sv))))

(defn find-recipe [recipe {:keys [board] :as state}]
  (cond
    (ends-with board recipe)
    (- (count board) (count recipe))
    (ends-with (subvec board 0 (dec (count board))) recipe)
    (- (count board) (count recipe) 1)
    :else
    (recur recipe (round state))))

(deftest find-recipe-test
  (is (= 9 (find-recipe [5 1 5 8 9] start-state)))
  (is (= 5 (find-recipe [0 1 2 4 5] start-state)))
  (is (= 18 (find-recipe [9 2 5 1 0] start-state)))
  (is (= 2018 (find-recipe [5 9 4 1 4] start-state))))

(deftest problem2
  (is (= 20283721 (find-recipe [1 4 7 0 6 1] start-state))))
