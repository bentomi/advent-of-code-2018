(ns advent-of-code-2018.day9.code)

(def players 405)
(def last-marble 71700)

(defn player [players play]
  (mod (dec play) players))

(defn play [players last-marble]
  (loop [to-play 1
         marbles (java.util.ArrayList. [0])
         current 0
         scores (vec (repeat players 0))]
    (if (> to-play last-marble)
      scores
      (let [size (.size marbles)]
        (if (pos? (mod to-play 23))
          (let [current' (if (= current (dec size)) 1 (+ current 2))]
            (.add marbles current' to-play)
            (recur (inc to-play)
                   marbles
                   current'
                   scores))
          (let [current' (mod (- current 7) size)
                win (+ to-play (.get marbles current'))]
            (.remove marbles current')
            (recur (inc to-play)
                   marbles
                   current'
                   (update scores (player players to-play) + win))))))))

(def problem1
  (apply max (play players last-marble)))
