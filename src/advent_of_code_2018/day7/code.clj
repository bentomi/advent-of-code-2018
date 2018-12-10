(ns advent-of-code-2018.day7.code)

(defonce input-file "src/advent_of_code_2018/day7/input.txt")

(defonce line-pattern #"Step (.+) must be finished before step (.+) can begin\.")

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> rdr line-seq
         (mapv #(subvec (re-matches line-pattern %) 1)))))

(defn edge-list [edges]
  (reduce (fn [graph [from to]] (update graph from (fnil conj #{}) to))
          {}
          edges))

(defn vertices [graph]
  (set (apply concat (keys graph) (vals graph))))

(defn no-in-edge
  ([graph] (no-in-edge graph (vertices graph)))
  ([graph vs]
   (filter (fn [vertex] (not-any? #(contains? % vertex) (vals graph)))
           vs)))

(defn top-sort [graph]
  (loop [graph graph
         unblocked (apply sorted-set (no-in-edge graph))
         sorted []]
    (if-let [v (first unblocked)]
      (let [graph' (dissoc graph v)]
        (recur graph'
               (into (disj unblocked v) (no-in-edge graph' (graph v)))
               (conj sorted v)))
      sorted)))

(def problem1
  (-> input edge-list top-sort clojure.string/join))

(defn end-time [step start-time]
  (+ start-time 60 (.codePointAt step 0)))

(defn parallel-schedule [full-graph workers]
  (loop [graph full-graph
         unblocked (apply sorted-set (no-in-edge graph))
         in-progress (sorted-set)
         elapsed 0
         step-times {}]
    (cond
      (and (seq unblocked) (< (count in-progress) workers))
      (let [v (first unblocked)]
        (recur graph
               (disj unblocked v)
               (conj in-progress [(end-time v elapsed) v])
               elapsed
               (assoc step-times v {:start elapsed})))
      (seq in-progress)
      (let [ending (first (partition-by first in-progress))
            end-time (ffirst ending)
            steps (map second ending)
            graph' (reduce dissoc graph steps)
            unblockables (for [s steps, v (graph s)] v)]
        (recur graph'
               (into (reduce disj unblocked steps)
                     (no-in-edge graph' unblockables))
               (apply disj in-progress ending)
               end-time
               (reduce (fn [step-times step]
                         (update step-times step assoc :end end-time))
                       step-times
                       steps)))
      :else
      step-times)))
