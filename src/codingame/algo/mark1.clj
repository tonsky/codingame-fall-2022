(ns codingame.algo.mark1
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

(defn move-units [game player]
  (let [left  (frequencies
                (for [{:keys [pos] :as tile} (tile-seq game)
                      :when (< (:x pos) (quot (:width game) 2))]
                  (:owner tile)))
        right (frequencies
                (for [{:keys [pos] :as tile} (tile-seq game)
                      :when (>= (:x pos) (quot (:width game) 2))]
                  (:owner tile)))
        foe   (opponent player)
        left? (> (/ (player left 1) (foe left 1))
                (/ (player right 1) (foe right 1)))
        targets (->> (tile-seq game)
                  (remove #(= player (:owner %)))
                  (remove :recycler?)
                  (remove #(= 0 (:scrap %))))]
    (for [tile (tile-seq game)
          :when (= player (:owner tile))
          :when (pos? (:units tile))
          :let [{:keys [pos]} tile
                target (->> targets
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
                                      (and (= foe (:owner %)) (= 0 (:units %)))    1
                                      (= :neutral (:owner %))                      2
                                      (= foe (:owner %))                           3
                                      (and (= player (:owner %)) (= 0 (:units %))) 4
                                      (= player (:owner %))                        5
                                      :else                                        6))
                              (* 10 (dist pos (:pos %)))
                              (- (:scrap %))))
                         first
                         :pos)]
          :when (some? target)]
      [:move player (:units tile) pos target])))

(defn recycler-places [game player]
  (let [{:keys [width]} game
        left  (frequencies
                (for [{:keys [pos] :as tile} (tile-seq game)
                      :when (< (:x pos) (quot width 2))]
                  (:owner tile)))
        right (frequencies
                (for [{:keys [pos] :as tile} (tile-seq game)
                      :when (>= (:x pos) (quot width 2))]
                  (:owner tile)))
        foe   (opponent player)
        left? (> (/ (player left 1) (foe left 1))
                (/ (player right 1) (foe right 1)))]
    (->>
      (for [{:keys [pos] :as tile} (tile-seq game)
            :when (= player (:owner tile))
            :when (= 0 (:units tile))
            :when (pos? (:scrap tile))
            :when (if left?
                    (>= (:x pos) (quot width 3))
                    (< (:x pos) (- width (quot width 3))))
            :let [around (->> (neighbour+self-pos game pos)
                           (map #(get-tile game %))
                           (remove #(recycled? game (:pos %)))
                           (remove :recycler?)
                           (remove #(= 0 (:scrap %)))
                           (map :scrap))
                  scrap  (reduce + 0 around)]
            :when (<= 5 (count around))
            :when (<= 12 scrap)]
        [scrap pos])
      (sort-by first)
      (reverse)
      (map second)
      (take 1))))

(defn spawn-units [game player scrap recycler-places]
  (let [recycler-places (set recycler-places)
        frontier (vec
                   (for [{:keys [pos] :as tile} (tile-seq game)
                         :when (and
                                 (pos? (:scrap tile))
                                 (not (:recycler? tile))
                                 (= player (:owner tile)))
                         :when (not (recycler-places pos))
                         :when (not (empty?
                                      (->> (neighbour-tiles game pos)
                                        (remove #(= 0 (:scrap %)))
                                        (remove #(= player (:owner %)))
                                        (remove :recycler?))))]
                     pos))
        distr (distribute (count frontier) (quot scrap 10))]
    (vec
      (for [[units [x y]] (zip distr frontier)
            :when (pos? units)]
        [:spawn player units (pos x y)]))))


(defn algo [player]
  (reify Algo
    (-move [_ game]
      (let [scrap           (-> game :scrap player)
            move-commands   (move-units game player)
            recycler-places (recycler-places game player)
            recyclers       (min
                              (count recycler-places)
                              (quot scrap 10))
            build-commands  (for [pos (take recyclers recycler-places)]
                              [:build player pos])
            scrap           (- scrap (* 10 recyclers))
            spawn-commands  (spawn-units game player scrap recycler-places)]
        (concat
          move-commands
          build-commands
          spawn-commands)))))
