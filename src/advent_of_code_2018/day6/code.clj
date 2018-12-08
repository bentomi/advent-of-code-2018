(ns advent-of-code-2018.day6.code)

(defonce input-file "src/advent_of_code_2018/day6/input.txt")

(defn parse-int [s]
  (Integer/parseInt s))

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> (line-seq rdr)
         (mapv #(let [[_ x y] (re-matches #"(\d+), (\d+)" %)]
                  [(parse-int x) (parse-int y)])))))

(defn corners [coords]
  (let [xs (map first coords), ys (map second coords)]
    [(apply min xs) (apply min ys)
     (apply max xs) (apply max ys)]))

(defn normalise-coords [coords]
  (let [[minx miny maxx maxy] (corners coords)]
    (mapv (fn [[x y]] [(- x minx) (- y miny)]) coords)))

(defonce normalised-input
  (normalise-coords input))

(defn distance [[x0 y0] [x1 y1]]
  (+ (Math/abs (- x1 x0)) (Math/abs (- y1 y0))))

(defn vec-of [n e]
  (vec (take n (repeat e))))

(defn closest-points [all-points coord]
  (->> all-points
       (map-indexed (fn [i point] [(distance point coord) i]))
       sort
       (partition-by first)
       first
       (mapv second)))

(defn normalised-grid [normalised-coords]
  (let [[_ _ maxx maxy] (corners normalised-coords)]
    (vec (for [y (range 0 (inc maxx))]
           (vec (for [x (range 0 (inc maxy))]
                  (closest-points normalised-coords [x y])))))))

(defonce grid
  (normalised-grid normalised-input))

(def border
  (let [[minx miny maxx maxy] (corners normalised-input)]
    (concat (for [y [0 maxy]
                  x (range 0 (inc maxx))]
              [x y])
            (for [x [0 maxx]
                  y (range 0 (inc maxy))]
              [x y]))))

(def infinite-areas
  (set
   (for [coord border
         :let [point-indexes (get-in grid coord)]
         :when (= 1 (count point-indexes))]
     (first point-indexes))))

(def problem1
  (->> (for [row grid
             [point & other-points] row
             :when (not (or (seq other-points) (infinite-areas point)))]
         point)
       frequencies
       (apply max-key second)
       second))
