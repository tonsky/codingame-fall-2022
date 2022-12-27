(ns codingame.algo.mark3
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

;; mark 3

(defn spawn-isolated [{:keys [player foe game clusters] :as plan}]
  (let [my-clusters (filter
                      #(and
                         (zero? (-> % :units player))
                         (zero? (-> % :units foe))
                         ;; can spawn
                         (pos? (-> % :tiles player))
                         ;; something to conquer
                         (pos? (+ (-> % :tiles :neutral) (-> % :tiles foe))))
                      clusters)]
    (reduce
      (fn [{:keys [handled scrap] :as plan} cluster]
        (cond+
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          :let [border (cluster-border game player cluster)]
          (empty? border) plan
          :let [pos (rand-nth border)]
          :else
          (-> plan
            (update :commands conj [:spawn player 1 pos])
            (update :handled conj pos)
            (update :scrap - 10))))
      plan
      my-clusters)))

(defn build [{:keys [player foe game clusters] :as plan}]
  (let [priority-tile (fn [pos]
                        (let [{:keys [scrap]} (get-tile game pos)]
                          (* scrap
                            (cond
                              (recycled? game pos foe)   3
                              (not (recycled? game pos)) 1
                              :else                      0))))
        priority-pos  (fn [tgt]
                        (transduce (map priority-tile) + 0 (neighbours+self game (:pos tgt))))
        my-clusters   (->> clusters
                        (filter #(zero? (-> % :tiles foe)))
                        (map :positions))
        middle        (->> (tile-seq game)
                        (filter #(<= (quot (:width game) 3) (:x (:pos %)) (* 2 (quot (:width game) 3))))
                        (filter #(and
                                   (= player (:owner %))
                                   (pos? (:scrap %))
                                   (zero? (:units %))
                                   (not (:recycler? %))
                                   (not (:dead? %))))
                        (remove (fn [tile] (some #(contains? % (:pos tile)) my-clusters)))
                        (sort-by #(- (priority-pos %))))]
    (reduce
      (fn [{:keys [game handled scrap] :as plan} {:keys [pos]}]
        (cond
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          ;; don’t build next to recycler
          (some #(:recycler? (get-tile game %)) (neighbours game pos)) plan
          :else
          (-> plan
            (update :game assoc-tile pos :recycler? true)
            (update :handled conj pos)
            (update :commands conj [:build player pos])
            (update :scrap - 10))))
      plan
      middle)))

(defn spawn [{:keys [player foe game clusters] :as plan}]
  (let [my-clusters (filter
                      (fn [cluster]
                        (and 
                          ;; foe has units
                          (pos? (-> cluster :units foe))
                          ;; less units than foe
                          (<=
                            (-> cluster :units player)
                            (-> cluster :units foe))
                          ;; can build, but not everything is mine
                          (< 0 (-> cluster :tiles player) (-> cluster :positions count))))
                      clusters)]
    (reduce
      (fn [{:keys [handled scrap] :as plan} cluster]
        (cond+
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          :let [border (->> (cluster-border game player cluster)
                         (remove handled)
                         shuffle)]
          (empty? border) plan
          :let [units  (min
                         (quot scrap 10)
                         (-> cluster :units foe (+ 2)))
                counts (distribute (count border) units)
                cmds   (map
                         (fn [pos cnt]
                           [:spawn player cnt pos])
                         border counts)]
          :else
          (-> plan
            (update :commands into cmds)
            (update :handled into (map #(nth % 3) cmds))
            (update :scrap - (* 10 (reduce #(+ %1 (nth %2 2)) 0 cmds))))))
      plan
      my-clusters)))

(defn move [{:keys [player foe game units clusters handled] :as plan}]
  (let [center   (pos (quot (:width game) 2) (quot (:height game) 2))
        priority (fn [pos]
                   (fn [target]
                     (let [{:keys [owner units]} (get-tile game target)]
                       (+
                         (* 20 (dist pos target))
                         (* 10 (cond
                                 (and (= foe owner) (zero? units)) 1
                                 (= :neutral owner) 2
                                 (= foe owner)      3
                                 :else              4))
                         (* 1 (dist center target))))))
        frontier (set
                   (for [tile      (tile-seq game)
                         :when     (= player (:owner tile))
                         neighbour (neighbours game (:pos tile))
                         :when     (not (handled neighbour))
                         :let      [neighbour (get-tile game neighbour)]
                         :when     (movable? neighbour)
                         :when     (not (:dead? neighbour))
                         :when     (not= player (:owner neighbour))
                         ;; don’t go to the tile that will be removed
                         :when     (or (not (recycled? game (:pos neighbour)))
                                     (> (:scrap neighbour) 1))]
                     (:pos neighbour)))]
    (reduce-kv
      (fn [plan pos units]
        (let [cluster (search #(contains? % pos) (map :positions clusters))]
          (if-some [targets (->> frontier
                              (filter #(cluster %))
                              (sort-by (priority pos))
                              (not-empty))]
            (let [cmds (map
                         (fn [target cnt]
                           [:move player cnt pos target])
                         targets (distribute (count targets) units))]
              (-> plan
                (update :commands into cmds)
                (update :units dissoc pos)))
            plan)))
      plan
      units)))

(defn spawn-rest [plan]
  (let [commands (->> (:commands plan)
                   (filter #(= :spawn (first %))))
        units    (quot (:scrap plan) 10)
        commands' (map
                    (fn [cmd units]
                      (assoc cmd 2 units))
                    commands
                    (distribute (count commands) units))]
    (-> plan
      (update :commands into commands')
      (update :scrap - (* 10 units)))))

(defn algo [player]
  (reify Algo
    (-move [_ game]
      (let [clusters (clusters game)
            units    (into {}
                       (for [{:keys [pos units owner]} (tile-seq game)
                             :when (= player owner)
                             :when (pos? units)]
                         [pos units]))
            foe      (opponent player)
            plan     (->
                       {:player   player
                        :foe      foe
                        :scrap    (-> game :scrap player)
                        :game     game
                        :units    units
                        :clusters clusters
                        :handled  #{}
                        :commands []}
                       (spawn-isolated)
                       (build)
                       (spawn)
                       (spawn-rest)
                       (move))]
        (:commands plan)))))
