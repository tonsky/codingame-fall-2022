(ns codingame.integration
  (:require
    [clojure.string :as str]
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

(defn algo [player]
  (reify Algo
    (-move [_ game]
      [])))

(defn read-longs []
  (-> (read-line)
    (str/split #" ")
    (->> (remove str/blank?)
      (mapv parse-long))))

(defn read-game [width height]
  (let [[scrap-me scrap-foe] (read-longs)
        *game (volatile! (make-game width height))]
    (reset-t!)
    (dotimes [y height]
      (dotimes [x width]
        (let [pos (pos x y)
              [scrap owner units recycler can-build can-spawn in-range-of-recycler] (read-longs)]
          (vswap! *game assoc-tile pos
            :scrap     scrap
            :owner     (case owner 1 :blue 0 :red -1 :neutral)
            :units     units
            :recycler? (= recycler 1)))))
    @*game))

(defmulti serialize first)

(defmethod serialize :build [[_ _ [x y]]]
  (format "BUILD %d %d" x y))

(defmethod serialize :move [[_ _ units from to]]
  (format "MOVE %d %d %d %d %d" units (:x from) (:y from) (:x to) (:y to)))
  
(defmethod serialize :spawn [[_ _ units [x y]]]
  (format "SPAWN %d %d %d" units x y))

(defn -main [& args]
  (let [[width height] (read-longs)
        algo  (algo :blue)]
    (while true
      (let [game     (read-game width height)
            commands (-move algo game)]
        (if (empty? commands)
          (println "WAIT")
          (->> commands
            (map serialize)
            (str/join ";")
            (println)))))))
