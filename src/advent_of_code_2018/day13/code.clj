(ns advent-of-code-2018.day13.code
  (:require [clojure.test :refer [deftest is]]))

(defonce input-file "src/advent_of_code_2018/day13/input.txt")

(defonce input
  (with-open [rdr (clojure.java.io/reader input-file)]
    (-> rdr line-seq vec)))

(def cart-direction
  {\< :left
   \> :right
   \^ :up
   \v :down})

(defn carts [input]
  (into (sorted-map)
        (for [y (range 0 (count input))
              :let [r (get input y)]
              x (range 0 (count r))
              :let [f (get r x)
                    d (cart-direction f)]
              :when d]
          [[y x] {:dir d, :intersections 0}])))

(defn tracks [input]
  (mapv (fn [r] (mapv #(or ({\< \-, \> \-, \^ \|, \v \|} %) %) r)) input))

(defn move [[y x] dir]
  (case dir
    :left [y (dec x)]
    :right [y (inc x)]
    :up [(dec y) x]
    :down [(inc y) x]))

(defn cart-after [{:keys [dir intersections] :as cart} rail-type]
  (case rail-type
    \\
    (case dir
      :left (assoc cart :dir :up)
      :right (assoc cart :dir :down)
      :up (assoc cart :dir :left)
      :down (assoc cart :dir :right))
    \/
    (case dir
      :left (assoc cart :dir :down)
      :right (assoc cart :dir :up)
      :up (assoc cart :dir :right)
      :down (assoc cart :dir :left))
    \+
    (assoc cart
           :dir (case (mod intersections 3)
                  0 (case dir :left :down, :right :up, :up :left, :down :right)
                  1 dir
                  2 (case dir :left :up, :right :down, :up :right, :down :left))
           :intersections (inc intersections))
    cart))

(defn move-cart [tracks cart-pos]
  (let [[pos {dir :dir :as cart}] cart-pos
        pos' (move pos dir)]
    [pos' (cart-after cart (get-in tracks pos'))]))

(defn move-carts [tracks carts crash-fn]
  (reduce (fn [carts [pos :as cart]]
            (if (contains? carts pos)
              (let [[pos' cart'] (move-cart tracks cart)]
                (if (contains? carts pos')
                  (crash-fn pos' carts cart)
                  (assoc (dissoc carts pos) pos' cart')))
              carts))
          carts
          carts))

(defn first-crash [tracks carts]
  (loop [carts carts]
    (let [carts' (move-carts tracks carts
                             (fn [pos carts cart] (reduced [:crash pos])))]
      (if (and (vector? carts') (= :crash (carts' 0)) )
        (carts' 1)
        (recur carts')))))

(deftest problem1
  (is (= [92 26] (first-crash (tracks input) (carts input)))))

(defn last-cart [tracks carts]
  (loop [carts carts]
    (if (= 1 (count carts))
      (first carts)
      (let [remove-carts (fn [pos carts [prev-pos]] (dissoc carts pos prev-pos))]
        (recur (move-carts tracks carts remove-carts))))))

(deftest problem2
  (is (= [18 86] (first (last-cart (tracks input) (carts input))))))
