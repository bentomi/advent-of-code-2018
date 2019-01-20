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
  (run! prn (take (+ 29 32) (execute program [1 0 0 0 0 0])))

  (run! prn (take 16 (drop 22 (execute program [1 0 0 0 0 0]))))

  (run! prn (take 32 (steps (subvec program 1)
                            {:ip-reg 1, :ip 4, :register [0 3 10551408 10551408 1 10551408]})))

  (run! prn (take 1000 (steps (subvec program 1)
                             {:ip-reg 1, :ip 3, :register [10551406 3 10551408 10551408 10551407 10551408]})))

  (prn (last (take-while some? (execute program [1 0 0 0 0 0]))))
  )
