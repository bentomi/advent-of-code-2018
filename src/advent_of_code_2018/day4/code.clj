(ns advent-of-code-2018.day4.code)

(defonce input-file "src/advent_of_code_2018/day4/input.txt")

(defonce line-pattern #"\[(\d+-\d+-\d+) (\d+):(\d+)\] (Guard #(\d+) begins shift|falls asleep|wakes up)")

(defn parse-int [s]
  (Integer/parseInt s))

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (-> rdr line-seq sort)))

(comment
  (every? #(let [[_ day hour minute action guard] (re-matches line-pattern %)]
             (or guard (= action "falls asleep") (= action "wakes up")))
          input)

  (every? #(let [[_ day hour minute action guard] (re-matches line-pattern %)]
             (or guard (zero? (parse-int hour))))
          input))

(defn normalized-action [action]
  (case action
    "falls asleep" :sleep
    "wakes up" :wake
    :begin))

(defonce actions
  (mapv #(let [[_ day hour minute action guard] (re-matches line-pattern %)
               base {:day day
                     :hour (parse-int hour)
                     :minute (parse-int minute)
                     :action (normalized-action action)}]
           (if guard (assoc base :guard (parse-int guard)) base))
        input))

(comment
  (every? (fn [[r0 r1]] (or (not= (:action r0) :sleep) (= (:action r1) :wake)))
          (partition 2 1 actions)))

(defonce shifts
  (let [complete (fn [[shifts current]]
                   (if current (conj shifts current) shifts))]
    (complete
     (reduce (fn [[shifts current :as state] r]
               (if (= :begin (:action r))
                 [(complete state) [r]]
                 [shifts (conj current r)]))
             [[] nil]
             actions))))

(comment
  (count shifts)
  (every? #(= :begin (-> % first :action)) shifts))

(defn shift-sleep
  [[begin & sleep-wakes]]
  [begin
   (reduce (fn [t [s w]] (+ t (- (:minute w) (:minute s))))
           0
           (partition 2 sleep-wakes))])

(comment
  (mapv shift-sleep shifts)

  (reduce (fn [m [{:keys [guard]} s]]
            (update m guard (fnil + 0) s))
          {}
          (map shift-sleep shifts)))

(defonce sleepiest-guard
  (-> (apply max-key second
             (reduce (fn [m [{:keys [guard]} s]]
                       (update m guard (fnil + 0) s))
                     {}
                     (map shift-sleep shifts)))
      first))

(defonce problem1
  (->> (for [[begin & sleep-wakes] shifts
             :when (= sleepiest-guard (:guard begin))
             [{s :minute} {w :minute}] (partition 2 sleep-wakes)
             m (range s w)]
         m)
       frequencies
       (apply max-key second)
       first
       (* sleepiest-guard)))

(defonce problem2
  (->> (for [[begin & sleep-wakes] shifts
             [{s :minute} {w :minute}] (partition 2 sleep-wakes)
             m (range s w)]
         [(:guard begin) m])
       frequencies
       (apply max-key second)
       first
       (apply *)))
