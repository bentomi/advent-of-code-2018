(ns advent-of-code-2018.day3.code)

(defonce input-file "src/advent_of_code_2018/day3/input.txt")

(defonce line-pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

(defn parse-int [s]
  (Integer/parseInt s))

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (mapv #(mapv parse-int (rest (re-matches line-pattern %)))
          (line-seq rdr))))

(defn overlap
  [claim0 claim1]
  (let [[_ tx0 ty0 w0 h0] claim0
        bx0 (+ tx0 w0)
        by0 (+ ty0 h0)
        [_ tx1 ty1 w1 h1] claim1
        bx1 (+ tx1 w1)
        by1 (+ ty1 h1)]
    [(max tx0 tx1) (max ty0 ty1) (min bx0 bx1) (min by0 by1)]))

(defn overlap-fields []
  (for [i (range (count input))
        :let [[one & others] (subvec input i)]
        other others
        :let [[tx ty bx by] (overlap one other)]
        x (range tx bx)
        y (range ty by)]
    [x y]))

(defonce problem1
  "The number of overlapping square inches."
  (-> (overlap-fields) set count))

(defn no-overlap?
  [claim0 claim1]
  (let [[tx ty bx by] (overlap claim0 claim1)]
    (or (>= tx bx) (>= ty by))))

(defonce problem2
  "The id of claim which does not overlap with any other claim."
  (for [one input :when (every? #(or (= one %) (no-overlap? one %)) input)]
    (nth one 0)))
