(ns advent-of-code-2018.day9.code)

(defn player [players play]
  (mod (dec play) players))

(defn value-of [{:keys [nodes]} current]
  (aget nodes (* 3 current)))

(defn prev-of [{:keys [nodes]} current]
  (aget nodes (+ (* 3 current) 1)))

(defn next-of [{:keys [nodes]} current]
  (aget nodes (+ (* 3 current) 2)))

(defn rewind [marbles current count]
  (if (pos? count)
    (recur marbles (prev-of marbles current) (dec count))
    current))

(defn add-after [{:keys [nodes new-node] :as marbles} current value]
  (let [current-offset (* 3 current)
        next (aget nodes (+ current-offset 2))
        next-offset (* 3 next)
        new-offset (* 3 new-node)]
    (aset-long nodes (+ current-offset 2) new-node) ; set next of current to new node
    (aset-long nodes (+ next-offset 1) new-node)    ; set prev of old next to new node
    (aset-long nodes new-offset value)              ; set value of new node
    (aset-long nodes (+ new-offset 1) current)      ; set prev of new node to current
    (aset-long nodes (+ new-offset 2) next))        ; set next of new node to old next
  (assoc marbles :new-node (inc new-node)))

(defn delete [{:keys [nodes] :as marbles} current]
  (let [current-offset (* 3 current)
        prev (aget nodes (+ current-offset 1))
        next (aget nodes (+ current-offset 2))]
    (aset-long nodes (+ (* 3 prev) 2) next)  ; set next of prev of current to next of current
    (aset-long nodes (+ (* 3 next) 1) prev)) ; set prev of next of current to prev of current
  marbles)

(defn play [players last-marble]
  (loop [to-play 1
         marbles {:nodes (long-array (* 3 last-marble)) :new-node 1}
         current 0
         scores (vec (repeat players 0))]
    (if (> to-play last-marble)
      scores
      (if (pos? (mod to-play 23))
        (let [target (next-of marbles current)]
          (recur (inc to-play)
                 (add-after marbles target to-play)
                 (next-of marbles target)
                 scores))
        (let [target (rewind marbles current 7)
              win (+ to-play (value-of marbles target))]
          (recur (inc to-play)
                 (delete marbles target)
                 (next-of marbles target)
                 (update scores (player players to-play) + win)))))))

(def players 405)

(def last-marble 71700)

(def problem1
  (apply max (play players last-marble)))

(def problem2
  (apply max (play players (* 100 last-marble))))
