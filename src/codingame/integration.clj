(ns codingame.integration
  (:require
    [clojure.string :as str]
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

(defn algo [player]
  (reify Algo
    (-move [_ game]
      [])))

;; integration

(defn read-longs []
  (-> (read-line)
    (str/split #" ")
    (->> (remove str/blank?)
      (mapv parse-long))))

(defn read-game [width height turn]
  (let [[scrap-me scrap-foe] (read-longs)]
    (measure "read-game"
      (let [*game (volatile! (make-game width height))]
        (reset-t!)
        (dotimes [y height]
          (dotimes [x width]
            (let [pos (pos x y)
                  [scrap owner units recycler _ _ _] (read-longs)]
              (vswap! *game assoc-tile pos
                :scrap     scrap
                :owner     ({1 :blue, 0 :red, -1 :neutral} owner)
                :units     units
                :recycler? (= recycler 1)))))
        (vswap! *game
          #(-> %
             (assoc
               :turn turn
               :scrap {:blue scrap-me, :red scrap-foe})
             (recalc-game)))))))

(defmulti serialize first)

(defmethod serialize :build [[_ _ [x y]]]
  (format "BUILD %d %d" x y))

(defmethod serialize :move [[_ _ units from to]]
  (format "MOVE %d %d %d %d %d" units (:x from) (:y from) (:x to) (:y to)))
  
(defmethod serialize :spawn [[_ _ units [x y]]]
  (format "SPAWN %d %d %d" units x y))

(defmethod serialize :message [[_ text]]
  (format "MESSAGE %s" text))

(defn -main [& _]
  (let [[width height] (read-longs)
        algo  (algo :blue)
        *turn (volatile! 0)]
    (while true
      (let [game     (read-game width height @*turn)
            t0       (now)
            commands (measure "turn time" (-move algo game))]
        (vswap! *turn inc)
        (if (empty? commands)
          (println "WAIT")
          (->> (conj commands [:message (format "time %d" (- (now) t0))])
            (map serialize)
            (str/join ";")
            (println)))))))
