(ns advent-of-code-2018.day2.code)

(defonce input-file "src/advent_of_code_2018/day2/input.txt")

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (vec (line-seq rdr))))

(defn has-letter-exactly?
  [letter-count letter-freqs]
  (some #(= letter-count (nth % 1)) letter-freqs))

(defn checksum
  [strings]
  (let [freqs (map frequencies strings)
        twos (->> freqs (filter (partial has-letter-exactly? 2)) count)
        threes (->> freqs (filter (partial has-letter-exactly? 3)) count)]
    (* twos threes)))

(defonce problem1 (checksum input))

(defn difference
  [string1 string2]
  (->> (map vector (range) string1 string2)
       (filter (fn [[_ l r]] (not= l r)))))

(comment
  (difference "abcde" "fghij")
  (difference "fghij" "fgfij"))

(defonce problem2
  "The 'pair' of codes differing only in one place with the differing
  letter removed."
  (for [i (range (count input))
        :let [[one & others] (subvec input i)]
        other others
        :let [diffs (difference one other)]
        :when (= 1 (count diffs))
        :let [[[i]] diffs]]
    (str (subs one 0 i) (subs one (inc i)))))
