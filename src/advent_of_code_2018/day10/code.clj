(ns advent-of-code-2018.day10.code)

(defonce input-file "src/advent_of_code_2018/day10/input.txt")

(defonce line-pattern #"position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>")

(defn parse-int [s]
  (Integer/parseInt s))

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> rdr line-seq
         (mapv #(mapv parse-int (drop 1 (re-matches line-pattern %)))))))

(defn frame-rect [stars]
  (let [xs (map first stars)
        ys (map second stars)]
    [(apply min xs) (apply min ys)
     (apply max xs) (apply max ys)]))

(defn frame-size [stars]
  (let [[minx miny maxx maxy] (frame-rect stars)]
    (* (- maxx minx -1) (- maxy miny -1))))

(defn move-star [[x y vx vy] seconds]
  [(+ x (* vx seconds)) (+ y (* vy seconds)) vx vy])

(defonce smallest-rect-position
  (-> (mapv #(move-star % 10227))))

(defonce smallest-picture
  (let [[dx dy] (frame-rect smallest-rect-position)]
    (mapv (fn [[x y vx vy]] [(- x dx) (- y dy)]) smallest-rect-position)))

(defn draw-picture [points]
  (let [width (inc (apply max (map first points)))
        height (inc (apply max (map second points)))
        to-draw (set points)]
    (doseq [y (range height)]
      (doseq [x (range width)]
        (print (if (to-draw [x y]) "*" " ")))
      (println))))

(draw-picture smallest-picture)
