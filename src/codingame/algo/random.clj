(ns codingame.algo.random
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

(defn algo [player]
  (reify Algo
    (-move [_ game]
      (let [scrap     (-> game :scrap player)
            buildable (->> (tile-seq game)
                        (filter #(and 
                                   (= player (:owner %))
                                   (= 0 (:units %))
                                   (not (:recycler? %))))
                        (map :pos))
            movable   (->> (tile-seq game)
                        (filter #(and 
                                   (= player (:owner %))
                                   (pos? (:units %)))))]
        (concat
          ;; move
          (keep
            (fn [tile]
              (let [ns (->> (neighbour-pos game (:pos tile))
                                 (filter #(pos? (:scrap (get-tile game %)))))]
                (when-not (empty? ns)
                  [:move player (:units tile) (:pos tile) (rand-nth ns)])))
            movable)
          ;; build-spawn
          (mapv
            (fn [pos _]
              (if (<= (rand) 0.5)
                [:build player pos]
                [:spawn player 1 pos]))
            (shuffle buildable)
            (range (quot scrap 10))))))))
      