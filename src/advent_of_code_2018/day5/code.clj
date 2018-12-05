(ns advent-of-code-2018.day5.code)

(defonce input-file "src/advent_of_code_2018/day5/input.txt")

(defonce input (slurp input-file))

(defn react [polymer]
  (let [b (StringBuilder. polymer)]
    (loop [i 0]
      (if (< i (dec (count b)))
        (if (= 32 (Math/abs (- (int (.charAt b i)) (int (.charAt b (inc i))))))
          (do (.delete b i (+ i 2))
              (recur (max (dec i) 0)))
          (recur (inc i)))
        (str b)))))

(react "dabAcCaCBAcCcaDA")

(count (react input))
