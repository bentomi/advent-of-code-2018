(ns advent-of-code-2018.day8.code)

(defonce input-file "src/advent_of_code_2018/day8/input.txt")

(defonce input
  (with-open [rdr (java.io.PushbackReader. (clojure.java.io/reader input-file))]
    (->> (repeatedly #(clojure.edn/read {:eof nil} rdr))
         (take-while integer?)
         vec)))

(defn sum-meta
  ([tree] (sum-meta tree 1 0))
  ([tree nodes sum]
   (if (and (pos? nodes) (seq tree))
     (let [[children metas & others] tree
           [tree sum] (sum-meta others children sum)
           [meta-vals tree] (split-at metas tree)]
       (recur tree (dec nodes) (apply + sum meta-vals)))
     [tree sum])))

(def problem1
  (sum-meta input))

(defn ref-value
  [children ref]
  (:value (nth children (dec ref) {:value 0})))

(declare parse-nodes)

(defn parse-node
  [tree]
  (when-let [[children metas & others] (seq tree)]
    (let [[rest-tree child-nodes] (parse-nodes others children [])
          [meta-vals rest-tree] (split-at metas rest-tree)
          values (if (seq child-nodes)
                   (map #(ref-value child-nodes %) meta-vals)
                   meta-vals)]
      [rest-tree {:children child-nodes, :metas meta-vals
                  :value (apply + values)}])))

(defn parse-nodes
  [tree nodes siblings]
  (if (and (pos? nodes) (seq tree))
    (let [[rest-tree node] (parse-node tree)]
      (recur rest-tree (dec nodes) (conj siblings node)))
    [tree siblings]))

(def problem2
  (-> input parse-node second :value))
