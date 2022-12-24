(ns codingame.mark1
  (:require
    [clojure.string :as str]
    [codingame.core :refer :all]))

(def ^:dynamic *input-fn*)

(def ^:dynamic *output-fn*)

;; parser

(defn read-longs []
  (-> (*input-fn*)
    (str/split #" ")
    (->> (remove str/blank?)
      (mapv parse-long))))

(defn read-game [width height]
  (let [[scrap-me scrap-foe] (read-longs)
        *arena (volatile! {})]
    (reset-t!)
    (dotimes [y height]
      (dotimes [x width]
        (let [pos (v2 x y)
              [scrap owner units recycler can-build can-spawn in-range-of-recycler] (read-longs)]
          (vswap! *arena assoc pos
            {:pos        pos
             :scrap      scrap
             :grass?     (= 0 scrap)
             :owner      (case owner 1 :me 0 :foe -1 :neutral)
             :units      units
             :recycler?  (= recycler 1)
             :build?     (= can-build 1)
             :spawn?     (= can-spawn 1)
             :collected? (= in-range-of-recycler 1)}))))
    {:scrap     scrap-me
     :scrap-foe scrap-foe
     :arena     @*arena}))

;; actions

(defn move-units [arena]
  (let [left  (frequencies
                (for [[pos place] arena
                      :when (< (:x pos) (quot width 2))]
                  (:owner place)))
        right (frequencies
                (for [[pos place] arena
                      :when (>= (:x pos) (quot width 2))]
                  (:owner place)))
        left? (> (/ (:me left 1) (:foe left 1))
                (/ (:me right 1) (:foe right 1)))
        targets (->> (vals arena)
                  (remove #(= :me (:owner %)))
                  (remove :recycler?)
                  (remove :grass?))]
    (for [[pos place] arena
          :when (= :me (:owner place))
          :when (> (:units place) 0)
          :let [target (->> targets
                         ;; TODO reachable
                         (sort-by
                           #(+
                              (* 100 (cond
                                       (if left?
                                         (> (:x (:pos %)) (:x pos))
                                         (< (:x (:pos %)) (:x pos)))
                                       1
                                       (= (:x pos) (:x (:pos %)))
                                       2
                                       :else
                                       3))
                              (* 30 (cond
                                      (and (= :foe (:owner %)) (= 0 (:units %))) 1
                                      (= :neutral (:owner %))                    2
                                      (= :foe (:owner %))                        3
                                      (and (= :me (:owner %)) (= 0 (:units %)))  4
                                      (= :me (:owner %))                         5
                                      :else                                      6))
                              (* 10 (dist pos (:pos %)))
                              (- (:scrap %))))
                         first
                         :pos)]
          :when (some? target)]
      (format "MOVE %d %d %d %d %d" (:units place) (:x pos) (:y pos) (:x target) (:y target)))))

(defn recycler-places [arena]
  (let [left  (frequencies
                (for [[pos place] arena
                      :when (< (:x pos) (quot width 2))]
                  (:owner place)))
        right (frequencies
                (for [[pos place] arena
                      :when (>= (:x pos) (quot width 2))]
                  (:owner place)))
        left? (> (/ (:me left 1) (:foe left 1))
                (/ (:me right 1) (:foe right 1)))]
    (->>
      (for [[pos place] arena
            :when (= :me (:owner place))
            :when (= 0 (:units place))
            :when (pos? (:scrap place))
            :when (if left?
                    (>= (:x pos) (quot width 3))
                    (< (:x pos) (- width (quot width 3))))
            :let [around (->> (cons pos (neighbours pos))
                           (map arena)
                           (remove :collected?)
                           (remove :recycler?)
                           (remove :grass?)
                           (map :scrap))
                  scrap  (reduce + 0 around)]
            :when (<= 5 (count around))
            :when (<= 12 scrap)]
        [scrap pos])
      (sort-by first)
      (reverse)
      (map second)
      (take 1))))

(defn spawn-units [arena scrap recycler-places]
  (let [recycler-places (set recycler-places)
        frontier (vec
                   (for [[pos place] arena
                         :when (:spawn? place)
                         :when (not (recycler-places pos))
                         :when (not (empty?
                                      (->> (neighbours pos)
                                        (map arena)
                                        (remove :grass?)
                                        (remove #(= :me (:owner %)))
                                        (remove :recycler?))))]
                     pos))
        distr (distribute (count frontier) (quot scrap 10))]
    (vec
      (for [[units [x y]] (zip distr frontier)
            :when (pos? units)]
        (format "SPAWN %d %d %d" units x y)))))

;; game loop

(defn run []
  (let [[w h] (read-longs)]
    (alter-var-root #'width (constantly w))
    (alter-var-root #'height (constantly h))
    (while true
      (let [{:keys [scrap arena]} (measure "read-game" (read-game width height))
            commands []
            commands (into commands
                       (measure "move-units"
                         (move-units arena)))
            recycler-places (measure "recycler-places"
                              (recycler-places arena))
            recyclers (min
                        (count recycler-places)
                        (quot scrap 10))
            commands  (into commands
                        (for [[x y] (take recyclers recycler-places)]
                          (format "BUILD %d %d" x y)))
            scrap     (- scrap (* 10 recyclers))
            commands  (into commands
                        (measure "spawn-units"
                          (spawn-units arena scrap recycler-places)))]
        (if (empty? commands)
          (*output-fn* "WAIT")
          (*output-fn* (str/join ";" commands)))))))

(defn -main [& args]
  (binding [*input-fn* read-line
            *output-fn* println]
    (run)))