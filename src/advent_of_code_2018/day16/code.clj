(ns advent-of-code-2018.day16.code
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]
            [loco.core :as loco]
            [loco.constraints :as constraints]
            [medley.core :as m]))

(defn parse-int [s]
  (Integer/parseInt s))

(def samples
  (with-open [rdr (io/reader "src/advent_of_code_2018/day16/samples.txt")]
    (->> rdr line-seq (remove #{""}) (map #(mapv parse-int (re-seq #"\d+" %)))
         (partition 3) (mapv vec))))

(defn regop [op register a b c]
  (assoc register c (op (register a) (register b))))

(defn immop [op register a b c]
  (assoc  register c (op (register a) b)))

(defn addr [register a b c] (regop + register a b c))
(defn addi [register a b c] (immop + register a b c))
(defn mulr [register a b c] (regop * register a b c))
(defn muli [register a b c] (immop * register a b c))
(defn banr [register a b c] (regop bit-and register a b c))
(defn bani [register a b c] (immop bit-and register a b c))
(defn borr [register a b c] (regop bit-or register a b c))
(defn bori [register a b c] (immop bit-or register a b c))
(defn setr [register a b c] (regop (fn [a b] a) register a b c))
(defn seti [register a b c] (assoc register c a))
(defn gtir [register a b c]
  (assoc  register c (if (> a (register b)) 1 0)))
(defn gtri [register a b c]
  (assoc register c (if (> (register a) b) 1 0)))
(defn gtrr [register a b c]
  (assoc register c (if (> (register a) (register b)) 1 0)))
(defn eqir [register a b c]
  (assoc register c (if (= a (register b)) 1 0)))
(defn eqri [register a b c]
  (assoc register c (if (= (register a) b) 1 0)))
(defn eqrr [register a b c]
  (assoc register c (if (= (register a) (register b)) 1 0)))

(def ops [addr addi mulr muli banr bani borr bori
          setr seti gtir gtri gtrr eqir eqri eqrr])

(defn matching-op [op [register-before [_opcode a b c] register-after]]
  (= register-after (op register-before a b c)))

(defn possible-ops [[_ [opcode] :as sample]]
  (let [iops (filter (fn [[_ op]] (matching-op op sample)) (m/indexed ops))]
    [opcode (set (map first iops))]))

(deftest problem1
  (is (= 529 (->> samples
                  (map possible-ops)
                  (filter #(< 2 (-> % second count)))
                  count))))

(defn assign [opcode-ops]
  (let [ins (mapv (fn [[opcode ops]] (constraints/$in [:opcode opcode] (vec ops)))
                  opcode-ops)
        vars (mapv #(-> [:opcode %]) (map first opcode-ops))
        model (conj ins (constraints/$distinct vars))
        solution (first (loco/solve model))]
    (reduce (fn [v op-code] (assoc v op-code (-> [:opcode op-code] solution ops)))
            []
            (range (count vars)))))

(def opcode-table
  (->> samples
       (map possible-ops)
       (group-by first)
       (m/map-vals (fn [opcode-ops]
                     (apply set/intersection (map second opcode-ops))))
       assign))

(def program
  (with-open [rdr (io/reader "src/advent_of_code_2018/day16/program.txt")]
    (->> rdr line-seq (mapv #(mapv parse-int (re-seq #"\d+" %))))))

(defn execute
  ([instructions] (execute instructions [0 0 0 0]))
  ([instructions register]
   (if-let [[[opcode a b c] & instructions] (seq instructions)]
     (recur instructions ((opcode-table opcode) register a b c))
     register)))

(deftest problem2
  (is (= [573 573 1 1] (execute program))))
