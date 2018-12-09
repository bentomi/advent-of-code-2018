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
