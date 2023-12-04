(ns advent-of-code.day-02
  (:require [clojure.string :as string]))

(def game-regex #"Game (\d+):(.+)")
(def pick-regex #"(\d+) (\w+)")
(def limits {"red" 12
             "green" 13
             "blue" 14})

(defn parse-pick [text]
  (->> text
       (re-seq pick-regex)
       (mapcat #(list (get % 2) (Integer/parseInt (get % 1))))
       (apply hash-map)))

(defn parse-picks [text]
  (as-> text _
    (string/split _ #";")
    (map parse-pick _)))

(defn parse-line [line]
  (as-> line _
    (re-find game-regex _)
    (rest _)
    {:game (Integer/parseInt (first _)) :picks (parse-picks (second _))}))

(defn is-valid [pick]
  (every?
   #(or (not (contains? pick %))
        (<= (get pick %) (get limits %)))
   (keys pick)))

(defn check-picks [picks]
  (every? #(is-valid %) picks))

(defn valid-picks [game]
  (-> game
      (get :picks)
      check-picks))

(defn highest-amount [picks key]
  (->> picks
       (filter #(contains? % key))
       (map #(get % key))
       (reduce max 1)))

(defn power [game]
  (as-> (get game :picks) _
    (map #(highest-amount _ %) (keys limits))
    (reduce * _)))

(defn part-1
  "Day 02 Part 1"
  [input]
  (->> input
       string/split-lines
       (map parse-line)
       (filter valid-picks)
       (map #(get % :game))
       (reduce +)))

(defn part-2
  "Day 02 Part 2"
  [input]
  (->> input
       string/split-lines
       (map parse-line)
       (map power)
       (reduce +)))
