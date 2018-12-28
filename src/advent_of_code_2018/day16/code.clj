(ns advent-of-code-2018.day16.code
  (:require [clojure.java.io :as io]
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

(defn regop [op {:keys [register] :as state} a b c]
  (assoc-in state [:register c] (op (register a) (register b))))

(defn immop [op {:keys [register] :as state} a b c]
  (assoc-in state [:register c] (op (register a) b)))

(defn addr [state a b c] (regop + state a b c))
(defn addi [state a b c] (immop + state a b c))
(defn mulr [state a b c] (regop * state a b c))
(defn muli [state a b c] (immop * state a b c))
(defn banr [state a b c] (regop bit-and state a b c))
(defn bani [state a b c] (immop bit-and state a b c))
(defn borr [state a b c] (regop bit-or state a b c))
(defn bori [state a b c] (immop bit-or state a b c))
(defn setr [state a b c] (regop (fn [a b] a) state a b c))
(defn seti [state a b c] (assoc-in state [:register c] a))
(defn gtir [{:keys [register] :as state} a b c]
  (assoc-in state [:register c] (if (> a (register b)) 1 0)))
(defn gtri [{:keys [register] :as state} a b c]
  (assoc-in state [:register c] (if (> (register a) b) 1 0)))
(defn gtrr [{:keys [register] :as state} a b c]
  (assoc-in state [:register c] (if (> (register a) (register b)) 1 0)))
(defn eqir [{:keys [register] :as state} a b c]
  (assoc-in state [:register c] (if (= a (register b)) 1 0)))
(defn eqri [{:keys [register] :as state} a b c]
  (assoc-in state [:register c] (if (= (register a) b) 1 0)))
(defn eqrr [{:keys [register] :as state} a b c]
  (assoc-in state [:register c] (if (= (register a) (register b)) 1 0)))

(def ops [addr addi mulr muli banr bani borr bori
          setr seti gtir gtri gtrr eqir eqri eqrr])

(defn matching-op [op [register-before [_opcode a b c] register-after]]
  (= register-after (:register (op {:register register-before} a b c))))

(deftest problem1
  (is (= 529 (->> samples
                  (map (fn [s] (count (filterv #(matching-op % s) ops))))
                  (filter #(< 2 %))
                  count))))
