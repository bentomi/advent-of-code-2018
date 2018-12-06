(ns advent-of-code-2018.day6.code)

(defonce input-file "src/advent_of_code_2018/day6/input.txt")

(defn parse-int [s]
  (Integer/parseInt s))

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (->> (line-seq rdr)
         (mapv #(let [[_ x y] (re-matches #"(\d+), (\d+)" %)]
                  [(parse-int x) (parse-int y)])))))

(comment
  (let [xs (map first input)
        ys (map second input)]
    [[(apply min xs) (apply max xs)]
     [(apply min ys) (apply max ys)]]))
