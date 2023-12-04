(ns advent-of-code.day-01
  (:require [clojure.string :as string]))

(def min-value (int \0))
(def max-value (int \9))

(defn number-or-nil [char]
  (let [code (int char)]
    (when (and
           (>= code min-value)
           (<= code max-value))
      char)))

(defn first-number [line]
  (some number-or-nil line))

(defn last-number [line]
  (some number-or-nil (reverse line)))

(defn line-value [line]
  (->> line
       (#(str (first-number %) (last-number %)))
       Integer/parseInt))

(defn values [lines]
  (map line-value lines))

(defn part-1
  "Day 01 Part 1"
  [input]
  (->> input
       string/split-lines
       values
       (reduce +)))

(def digits {"one" 1
             "two" 2
             "three" 3
             "four" 4
             "five" 5
             "six" 6
             "seven" 7
             "eight" 8
             "nine" 9})


(def digit-regex
  (as-> digits _
    (keys _)
    (string/join "|" _)
    (str "(?=(\\d|" _ "))")
    (re-pattern _)))

(defn to-digit [value]
  (if (contains? digits value)
    (get digits value)
    value))

(defn get-result-value [values]
  (Integer/parseInt (str (first values) (last values))))

(defn real-line-value [line]
  (->> line
       (re-seq digit-regex)
       (map last)
       (map to-digit)
       get-result-value))

(defn real-values [lines]
  (map real-line-value lines))

(defn part-2
  "Day 01 Part 2"
  [input]
  (->> input
       string/split-lines
       real-values
       (reduce +)))
