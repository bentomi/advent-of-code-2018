(ns advent-of-code-2018.day15.code-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [advent-of-code-2018.day15.code :refer :all]))

(defn parse-field [drawing]
  (->> drawing
       str/split-lines
       (map str/trim)
       (remove #{""})
       (mapv vec)))

(deftest field1-test
  (let [input (parse-field "
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######")
        initial-state {:field input :warriors (warriors input)
                       :attack-powers {\E 15, \G 3}}]
    (is (= 4988 (battle-outcome-without-elf-death initial-state)))))

(deftest field2-test
  (let [input (parse-field "
#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######")
        initial-state {:field input :warriors (warriors input)
                       :attack-powers {\E 3, \G 3}}
        [state full-rounds :as battle-result] (battle initial-state {:trace false})
        advanced-state (assoc-in initial-state [:attack-powers \E] 4)
        [astate afull-rounds :as abattle-result] (battle advanced-state)]
    (let [pos [1 1]
          ws (:warriors initial-state)
          field (:field initial-state)
          field-without (assoc-in field pos \.)
          targets (find-targets (get-in field pos) ws)
          in-range (set (mapcat #(free-adjacent field-without %) targets))]
      (is (= [[1 5] [2 3] [4 1]] targets))
      (is (= #{[1 3] [2 4] [4 2] [5 1]} in-range))
      (is (some? (bfs field pos in-range))))
    (is (= [1 2] (warrior-move initial-state [1 1])))
    (is (= 46 full-rounds))
    (is (= (sorted-map [1 2] {:race \E :hp 164} [1 4] {:race \E :hp 197}
                       [2 3] {:race \E :hp 200}
                       [3 1] {:race \E :hp 98}
                       [4 2] {:race \E :hp 200})
           (:warriors state)))
    (is (= 39514 (battle-outcome battle-result)))
    (is (= 33 afull-rounds))
    (is (= (sorted-map [1 2] {:race \E :hp 200} [1 4] {:race \E :hp 23}
                       [2 3] {:race \E :hp 200}
                       [3 1] {:race \E :hp 125} [3 5] {:race \E :hp 200}
                       [4 2] {:race \E :hp 200})
           (:warriors astate)))
    (is (= 31284 (battle-outcome abattle-result)))))

(deftest field3-test
  (let [input (parse-field "
#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######")
        initial-state {:field input :warriors (warriors input)
                       :attack-powers {\E 3, \G 3}}]
    (is (= 27755 (battle-outcome (battle initial-state))))
    (is (nil? (battle-outcome-without-elf-death
               (assoc-in initial-state [:attack-powers \E] 14))))
    (is (= 3478 (battle-outcome-without-elf-death
                 (assoc-in initial-state [:attack-powers \E] 15))))))

(deftest field4-test
  (let [input (parse-field "
#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######")
        initial-state {:field input :warriors (warriors input)
                       :attack-powers {\E 3, \G 3}}]
    (is (= 28944 (battle-outcome (battle initial-state))))
    (is (nil? (battle-outcome-without-elf-death
               (assoc-in initial-state [:attack-powers \E] 11))))
    (is (= 6474 (battle-outcome-without-elf-death
                 (assoc-in initial-state [:attack-powers \E] 12))))))

(deftest field5-test
  (let [input (parse-field "
#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########")
        initial-state {:field input :warriors (warriors input)
                       :attack-powers {\E 3, \G 3}}]
    (is (= 18740 (battle-outcome (battle initial-state))))
    (is (nil? (battle-outcome-without-elf-death
               (assoc-in initial-state [:attack-powers \E] 33))))
    (is (= 1140 (battle-outcome-without-elf-death
                 (assoc-in initial-state [:attack-powers \E] 34))))))

(deftest move-test
  (let [input (parse-field "
################################
#...############################
###..###########################
##..G..#########################
#......#########################
##......########################
#.......########################
###..G.#########################
###.G..#########################
######.G.#######################
#######....#####################
###..#.G..G.GE........##########
##.........G..#####...##.#######
#............#######...#..######
#...####....#########......#####
#...##.#..G.#########.......####
#...##....EG#########.........##
#...##...E.E#########.......####
#.......GE..#########.......####
#.......E....#######...........#
#.....GE......#####..EE........#
#.................E..........###
#................#####.#..######
#..#..............####...#######
#..#..............######.#######
####.#...........###############
########..##...#################
##...##..###..##################
#.......########################
##......########################
###......#######################
################################")
        initial-state {:field input :warriors (warriors input)
                       :attack-powers {\E 3, \G 3}}]
    (is (= [12 10] (warrior-move initial-state [11 10])))))
