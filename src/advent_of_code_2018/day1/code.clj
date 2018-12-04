(ns advent-of-code-2018.day1.code)

(defonce input-file "src/advent_of_code_2018/day1/input.txt")

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (mapv #(Integer/parseInt %) (line-seq rdr))))

(defonce problem1
  "The frequency reached from input."
  (reduce + input))

(defonce problem2
  "The first frequency seen twice whith repeated input or the set of
  frequencies seen if all are unique.

  Blows up, if the set of frequencies before a duplicate is too
  large."
  (reduce (fn [seen freq]
            (if (seen freq)
              (reduced freq)
              (conj seen freq)))
          #{0}
          (reductions + (cycle input))))
