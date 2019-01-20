(ns advent-of-code-2018.day19.code
    (:require [clojure.java.io :as io]
              [clojure.string :as str]
              [clojure.test :refer [deftest is]]
              [medley.core :as m]))

(defn parse-int [s]
  (Integer/parseInt s))

(defn regop [op register a b c]
  (assoc register c (op (register a) (register b))))

(defn immop [op register a b c]
  (assoc register c (op (register a) b)))

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
  (assoc register c (if (> a (register b)) 1 0)))
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

(def name->op
  (into {"#ip" :bind-ip}
        (map #(-> [(name %) (-> % resolve deref)])
             `[addr addi mulr muli banr bani borr bori
               setr seti gtir gtri gtrr eqir eqri eqrr])))

(def program
  (with-open [rdr (io/reader "src/advent_of_code_2018/day19/input.txt")]
    (->> rdr line-seq (mapv #(let [[op & args] (str/split % #"\s")]
                               (into [(name->op op)] (map parse-int args)))))))

(defn exec-op [register [op & args]]
  (apply op register args))

(defn write-ip-reg [{:keys [ip ip-reg] :as state}]
  (cond-> state
    (some? ip-reg) (assoc-in [:register ip-reg] ip)))

(defn read-ip-reg [{:keys [ip-reg] :as state}]
  (cond-> state
    (some? ip-reg) (assoc :ip (get-in state [:register ip-reg]))))

(defn exec-instruction [state [op :as instr]]
  (-> state
      write-ip-reg
      (update :register exec-op instr)
      read-ip-reg
      (update :ip inc)))

(defn prepare [[[op arg] :as program]]
  (if (= :bind-ip op)
    [(subvec program 1) {:ip-reg arg}]
    [program {}]))

(defn steps [program state]
  (iterate #(when-let [instr (get program (:ip %))]
              (exec-instruction % instr))
           state))

(defn execute
  ([program] (execute program [0 0 0 0 0 0]))
  ([program register]
   (let [[program state] (prepare program)]
     (steps program (assoc state :ip 0 :register register)))))

(deftest problem1-test
  (let [end-state (last (take-while some? (execute program)))]
    (is (= 3224 (get-in end-state [:register 0])))))

(comment
  (let [p (subvec program 1)]
    (doseq [s (take-while some? (execute program))]
      (prn (:ip s) (-> s :ip p))))

  (run! prn (take (+ 29 32) (execute program [1 0 0 0 0 0])))

  (run! prn (take 16 (drop 21 (execute program [1 0 0 0 0 0]))))

  {:ip-reg 1, :ip 3, :register [0 2 1 10551408 1 10550400]}
  {:ip-reg 1, :ip 4, :register [0 3 1 10551408 1 1]}
  {:ip-reg 1, :ip 5, :register [0 4 1 10551408 1 0]}
  {:ip-reg 1, :ip 6, :register [0 5 1 10551408 1 0]}
  {:ip-reg 1, :ip 8, :register [0 7 1 10551408 1 0]}
  {:ip-reg 1, :ip 9, :register [0 8 2 10551408 1 0]}
  {:ip-reg 1, :ip 10, :register [0 9 2 10551408 1 0]}
  {:ip-reg 1, :ip 11, :register [0 10 2 10551408 1 0]}
  {:ip-reg 1, :ip 3, :register [0 2 2 10551408 1 0]}
  {:ip-reg 1, :ip 4, :register [0 3 2 10551408 1 2]}
  {:ip-reg 1, :ip 5, :register [0 4 2 10551408 1 0]}
  {:ip-reg 1, :ip 6, :register [0 5 2 10551408 1 0]}
  {:ip-reg 1, :ip 8, :register [0 7 2 10551408 1 0]}
  {:ip-reg 1, :ip 9, :register [0 8 3 10551408 1 0]}
  {:ip-reg 1, :ip 10, :register [0 9 3 10551408 1 0]}
  {:ip-reg 1, :ip 11, :register [0 10 3 10551408 1 0]}

  (run! prn (take 24 (steps (subvec program 1)
                            {:ip-reg 1, :ip 9, :register [0 8 10551408 10551408 1 0]})))

  {:ip-reg 1, :ip 9, :register [0 8 10551408 10551408 1 0]}
  {:ip-reg 1, :ip 10, :register [0 9 10551408 10551408 1 0]}
  {:ip-reg 1, :ip 11, :register [0 10 10551408 10551408 1 0]}
  {:ip-reg 1, :ip 3, :register [0 2 10551408 10551408 1 0]}
  {:ip-reg 1, :ip 4, :register [0 3 10551408 10551408 1 10551408]}
  {:ip-reg 1, :ip 5, :register [0 4 10551408 10551408 1 1]}
  {:ip-reg 1, :ip 7, :register [0 6 10551408 10551408 1 1]}
  {:ip-reg 1, :ip 8, :register [1 7 10551408 10551408 1 1]}
  {:ip-reg 1, :ip 9, :register [1 8 10551409 10551408 1 1]}
  {:ip-reg 1, :ip 10, :register [1 9 10551409 10551408 1 1]}
  {:ip-reg 1, :ip 12, :register [1 11 10551409 10551408 1 1]}
  {:ip-reg 1, :ip 13, :register [1 12 10551409 10551408 2 1]}
  {:ip-reg 1, :ip 14, :register [1 13 10551409 10551408 2 0]}
  {:ip-reg 1, :ip 15, :register [1 14 10551409 10551408 2 0]}
  {:ip-reg 1, :ip 2, :register [1 1 10551409 10551408 2 0]}
  {:ip-reg 1, :ip 3, :register [1 2 1 10551408 2 0]}

  (run! prn (take 24 (steps (subvec program 1)
                            {:ip-reg 1, :ip 9, :register [3 8 10551408 10551408 3 0]})))

  {:ip-reg 1, :ip 9, :register [3 8 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 10, :register [3 9 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 11, :register [3 10 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 3, :register [3 2 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 4, :register [3 3 10551408 10551408 3 31654224]}
  {:ip-reg 1, :ip 5, :register [3 4 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 6, :register [3 5 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 8, :register [3 7 10551408 10551408 3 0]}
  {:ip-reg 1, :ip 9, :register [3 8 10551409 10551408 3 0]}
  {:ip-reg 1, :ip 10, :register [3 9 10551409 10551408 3 1]}
  {:ip-reg 1, :ip 12, :register [3 11 10551409 10551408 3 1]}
  {:ip-reg 1, :ip 13, :register [3 12 10551409 10551408 4 1]}
  {:ip-reg 1, :ip 14, :register [3 13 10551409 10551408 4 0]}
  {:ip-reg 1, :ip 15, :register [3 14 10551409 10551408 4 0]}
  {:ip-reg 1, :ip 2, :register [3 1 10551409 10551408 4 0]}
  {:ip-reg 1, :ip 3, :register [3 2 1 10551408 4 0]}
  {:ip-reg 1, :ip 4, :register [3 3 1 10551408 4 4]}
  {:ip-reg 1, :ip 5, :register [3 4 1 10551408 4 0]}
  {:ip-reg 1, :ip 6, :register [3 5 1 10551408 4 0]}
  {:ip-reg 1, :ip 8, :register [3 7 1 10551408 4 0]}
  {:ip-reg 1, :ip 9, :register [3 8 2 10551408 4 0]}
  {:ip-reg 1, :ip 10, :register [3 9 2 10551408 4 0]}
  {:ip-reg 1, :ip 11, :register [3 10 2 10551408 4 0]}
  {:ip-reg 1, :ip 3, :register [3 2 2 10551408 4 0]}

  (run! prn (take 32 (steps (subvec program 1)
                            {:ip-reg 1, :ip 9, :register [3 8 10551408 10551408 10551407 0]})))

  {:ip-reg 1, :ip 9, :register [3 8 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 10, :register [3 9 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 11, :register [3 10 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 3, :register [3 2 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 4, :register [3 3 10551408 10551408 10551407 111332200231056]}
  {:ip-reg 1, :ip 5, :register [3 4 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 6, :register [3 5 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 8, :register [3 7 10551408 10551408 10551407 0]}
  {:ip-reg 1, :ip 9, :register [3 8 10551409 10551408 10551407 0]}
  {:ip-reg 1, :ip 10, :register [3 9 10551409 10551408 10551407 1]}
  {:ip-reg 1, :ip 12, :register [3 11 10551409 10551408 10551407 1]}
  {:ip-reg 1, :ip 13, :register [3 12 10551409 10551408 10551408 1]}
  {:ip-reg 1, :ip 14, :register [3 13 10551409 10551408 10551408 0]}
  {:ip-reg 1, :ip 15, :register [3 14 10551409 10551408 10551408 0]}
  {:ip-reg 1, :ip 2, :register [3 1 10551409 10551408 10551408 0]}
  {:ip-reg 1, :ip 3, :register [3 2 1 10551408 10551408 0]}
  {:ip-reg 1, :ip 4, :register [3 3 1 10551408 10551408 10551408]}
  {:ip-reg 1, :ip 5, :register [3 4 1 10551408 10551408 1]}
  {:ip-reg 1, :ip 7, :register [3 6 1 10551408 10551408 1]}
  {:ip-reg 1, :ip 8, :register [10551411 7 1 10551408 10551408 1]}
  {:ip-reg 1, :ip 9, :register [10551411 8 2 10551408 10551408 1]}
  {:ip-reg 1, :ip 10, :register [10551411 9 2 10551408 10551408 0]}
  {:ip-reg 1, :ip 11, :register [10551411 10 2 10551408 10551408 0]}
  {:ip-reg 1, :ip 3, :register [10551411 2 2 10551408 10551408 0]}
  {:ip-reg 1, :ip 4, :register [10551411 3 2 10551408 10551408 21102816]}
  {:ip-reg 1, :ip 5, :register [10551411 4 2 10551408 10551408 0]}
  {:ip-reg 1, :ip 6, :register [10551411 5 2 10551408 10551408 0]}
  {:ip-reg 1, :ip 8, :register [10551411 7 2 10551408 10551408 0]}
  {:ip-reg 1, :ip 9, :register [10551411 8 3 10551408 10551408 0]}
  {:ip-reg 1, :ip 10, :register [10551411 9 3 10551408 10551408 0]}
  {:ip-reg 1, :ip 11, :register [10551411 10 3 10551408 10551408 0]}
  {:ip-reg 1, :ip 3, :register [10551411 2 3 10551408 10551408 0]}

  (run! prn (take 32 (steps (subvec program 1)
                            {:ip-reg 1, :ip 9, :register [10551411 8 10551407 10551408 10551408 0]})))

  {:ip-reg 1, :ip 9, :register [10551411 8 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 10, :register [10551411 9 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 11, :register [10551411 10 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 3, :register [10551411 2 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 4, :register [10551411 3 10551407 10551408 10551408 111332200231056]}
  {:ip-reg 1, :ip 5, :register [10551411 4 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 6, :register [10551411 5 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 8, :register [10551411 7 10551407 10551408 10551408 0]}
  {:ip-reg 1, :ip 9, :register [10551411 8 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 10, :register [10551411 9 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 11, :register [10551411 10 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 3, :register [10551411 2 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 4, :register [10551411 3 10551408 10551408 10551408 111332210782464]}
  {:ip-reg 1, :ip 5, :register [10551411 4 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 6, :register [10551411 5 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 8, :register [10551411 7 10551408 10551408 10551408 0]}
  {:ip-reg 1, :ip 9, :register [10551411 8 10551409 10551408 10551408 0]}
  {:ip-reg 1, :ip 10, :register [10551411 9 10551409 10551408 10551408 1]}
  {:ip-reg 1, :ip 12, :register [10551411 11 10551409 10551408 10551408 1]}
  {:ip-reg 1, :ip 13, :register [10551411 12 10551409 10551408 10551409 1]}
  {:ip-reg 1, :ip 14, :register [10551411 13 10551409 10551408 10551409 1]}
  {:ip-reg 1, :ip 16, :register [10551411 15 10551409 10551408 10551409 1]}
  {:ip-reg 1, :ip 257, :register [10551411 256 10551409 10551408 10551409 1]}
  nil

  (->> (steps (subvec program 1)
              {:ip-reg 1, :ip 9, :register [0 8 10551408 10551408 1 0]})

       (drop-while #(< (get-in % [:register 0]) 4))
       (take 32)
       (run! prn))

  {:ip-reg 1, :ip 8, :register [6 7 3517136 10551408 3 1]}
  {:ip-reg 1, :ip 9, :register [6 8 3517137 10551408 3 1]}
  {:ip-reg 1, :ip 10, :register [6 9 3517137 10551408 3 0]}
  {:ip-reg 1, :ip 11, :register [6 10 3517137 10551408 3 0]}
  {:ip-reg 1, :ip 3, :register [6 2 3517137 10551408 3 0]}
  {:ip-reg 1, :ip 4, :register [6 3 3517137 10551408 3 10551411]}
  {:ip-reg 1, :ip 5, :register [6 4 3517137 10551408 3 0]}
  {:ip-reg 1, :ip 6, :register [6 5 3517137 10551408 3 0]}
  {:ip-reg 1, :ip 8, :register [6 7 3517137 10551408 3 0]}
  {:ip-reg 1, :ip 9, :register [6 8 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 10, :register [6 9 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 11, :register [6 10 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 3, :register [6 2 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 4, :register [6 3 3517138 10551408 3 10551414]}
  {:ip-reg 1, :ip 5, :register [6 4 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 6, :register [6 5 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 8, :register [6 7 3517138 10551408 3 0]}
  {:ip-reg 1, :ip 9, :register [6 8 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 10, :register [6 9 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 11, :register [6 10 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 3, :register [6 2 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 4, :register [6 3 3517139 10551408 3 10551417]}
  {:ip-reg 1, :ip 5, :register [6 4 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 6, :register [6 5 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 8, :register [6 7 3517139 10551408 3 0]}
  {:ip-reg 1, :ip 9, :register [6 8 3517140 10551408 3 0]}
  {:ip-reg 1, :ip 10, :register [6 9 3517140 10551408 3 0]}
  {:ip-reg 1, :ip 11, :register [6 10 3517140 10551408 3 0]}
  {:ip-reg 1, :ip 3, :register [6 2 3517140 10551408 3 0]}
  {:ip-reg 1, :ip 4, :register [6 3 3517140 10551408 3 10551420]}
  {:ip-reg 1, :ip 5, :register [6 4 3517140 10551408 3 0]}
  {:ip-reg 1, :ip 6, :register [6 5 3517140 10551408 3 0]}

)
