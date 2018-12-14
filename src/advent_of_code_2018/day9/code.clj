(ns advent-of-code-2018.day9.code
  (:require [clojure.core.rrb-vector :as fv]))

(set! *warn-on-reflection* true)

(def players 405)
(def last-marble 71700)

(defn player [players play]
  (mod (dec play) players))

(defn add [v i e]
  (when (and (= i 95) (= 1113 (count v)))
    (prn i (count v) v))
  (let [v' (assoc (fv/subvec v 0 i) i e)]
      (if (= i (count v))
        v'
        (fv/catvec v' (fv/subvec v i)))))

(defn delete [v i]
  (fv/catvec (fv/subvec v 0 i) (fv/subvec v (inc i))))

(defn play [players last-marble]
  (loop [to-play 1
         marbles (fv/vector-of :long 0)
         current 0
         scores (vec (repeat players 0))]
    (if (> to-play last-marble)
      scores
      (let [size (count marbles)]
        (if (pos? (mod to-play 23))
          (let [current' (if (= current (dec size)) 1 (+ current 2))]
            (recur (inc to-play)
                   (add marbles current' to-play)
                   current'
                   scores))
          (let [current' (mod (- current 7) size)
                win (+ to-play (get marbles current'))]
            (recur (inc to-play)
                   (delete marbles current')
                   (long current')
                   (update scores (player players to-play) + win))))))))

(comment

(defonce problem1
  (apply max (play players last-marble)))

(defonce problem2
  (apply max (play players (* last-marble 100))))
  (apply max (play  9   25)) ;; 32
  (apply max (play 10  161)) ;; 8317
  (apply max (play 13 7999)) ;; 146373
  (apply max (play 17 1104)) ;; 2764
  (apply max (play 21 6111)) ;; 54718
  (apply max (play 30 5807)) ;; 37305


  (apply max (play players (* last-marble 1/100)))
  (apply max (play players (* last-marble 1/10)))
  (apply max (play players (* last-marble 1)))
  (apply max (play players (* last-marble 10)))
  (apply max (play players (* last-marble 100))))
