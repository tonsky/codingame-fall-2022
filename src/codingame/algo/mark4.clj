(ns codingame.algo.mark4
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

;; mark 4

(defn defend [{:keys [player foe game clusters] :as plan}]
  (let [need-def (->>
                   (for [tile (tile-seq game)
                       :when (= player (:owner tile))
                       :when (not (:recycler? tile))
                       :let  [ns  (neighbour-pos game (:pos tile))
                              sur (->> ns
                                    (map #(get-tile game %))
                                    (filter #(= foe (:owner %)))
                                    (map :units)
                                    (reduce + 0))]
                         :when (< (:units tile) sur)]
                     [tile sur])
                   (sort-by (fn [[tile sur]] (- (:units tile) sur))))]
    (reduce
      (fn [{:keys [handled scrap] :as plan} [{:keys [pos units] :as tile} sur]]
        (cond+
          (handled pos) plan
          (< scrap 10) (reduced plan)
          :else
          (let [units (min
                        (quot scrap 10)
                        (+ 2 (- sur units)))]
            (-> plan
              (update :commands conj [:spawn player units pos])
              (update :handled conj pos)
              (update :scrap - (* 10 units))))))
      plan
      need-def)))

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
          (< scrap 10)  (reduced plan)
          :let [border (cluster-border game player cluster)]
          (empty? border) plan
          :let [{:keys [pos]} (rand-nth border)]
          :else
          (-> plan
            (update :commands conj [:spawn player 1 pos])
            (update :handled conj pos)
            (update :scrap - 10))))
      plan
      my-clusters)))

(defn build [{:keys [player foe side game clusters] :as plan}]
  (let [priority-tile (fn [tile]
                        (* (:scrap tile)
                          (cond
                            (recycled? game tile foe)   3
                            (not (recycled? game tile)) 1
                            :else                       0)))
        priority-nbs  (fn [tile]
                         (transduce (map priority-tile) + 0 (neighbour+self-tiles game tile)))
        my-clusters   (->> clusters
                        (filter #(zero? (-> % :tiles foe)))
                        (map :positions))
        buildable     (->> (tile-seq game)
                        (filter #(case side
                                   :left  (>= (:x (:pos %)) (- (quot (:width game) 2) 1))
                                   :right (<= (:x (:pos %)) (+ (quot (:width game) 2) 1))))
                        (filter #(and
                                   (= player (:owner %))
                                   (pos? (:scrap %))
                                   (zero? (:units %))
                                   (not (:recycler? %))
                                   (not (:dead? %))))
                        (remove (fn [tile] (some #(contains? % (:pos tile)) my-clusters)))
                        (sort-by #(- (priority-nbs %))))]
    (reduce
      (fn [{:keys [game handled scrap] :as plan} {:keys [pos]}]
        (cond
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          ;; don???t build next to recycler
          (some #(:recycler? (get-tile game %)) (neighbour-pos game pos)) plan
          :else
          (-> plan
            (update :game assoc-tile pos :recycler? true)
            (update :handled conj pos)
            (update :commands conj [:build player pos])
            (update :scrap - 10))))
      plan
   buildable)))

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
          (< scrap 10)  (reduced plan)
          :let [border (->> (cluster-border game player cluster)
                         (remove #(handled (:pos %)))
                         shuffle)]
          (empty? border) plan
          :let [units  (min
                         (quot scrap 10)
                         (-> cluster :units foe (+ 2)))
                counts (distribute (count border) units)
                cmds   (map
                         (fn [tile cnt]
                           [:spawn player cnt (:pos tile)])
                         border counts)]
          :else
          (-> plan
            (update :commands into cmds)
            (update :handled into (map #(nth % 3) cmds))
            (update :scrap - (* 10 (reduce #(+ %1 (nth %2 2)) 0 cmds))))))
      plan
      my-clusters)))

(defn move [{:keys [player foe game units clusters handled] :as plan}]
  (let [center   (quot (:width game) 2)
        priority (fn [pos]
                   (fn [target]
                     (let [{:keys [owner units]} target]
                       (+
                         (* 20 (dist (:pos pos) (:pos target)))
                         (* 10 (cond
                                 (and (= foe owner) (pos? units))  1
                                 (= :neutral owner)                2
                                 (and (= foe owner) (zero? units)) 3
                                 :else                             4))
                         (* 1 (abs (- (:x (:pos target)) center)))))))
        frontier (set
                   (for [tile      (tile-seq game)
                         :when     (= player (:owner tile))
                         neighbour (neighbour-tiles game tile)
                         :when     (not (handled (:pos neighbour)))
                         :when     (movable? neighbour)
                         :when     (not (:dead? neighbour))
                         :when     (not= player (:owner neighbour))
                         ;; don???t go to the tile that will be removed
                         :when     (or (not (recycled? game neighbour))
                                     (> (:scrap neighbour) 1))]
                     neighbour))]
    (reduce-kv
      (fn [{:keys [handled] :as plan} pos units]
        (let [tile    (get-tile game pos)
              cluster (search #(contains? % tile) (map :positions clusters))]
          (if-some [targets (->> frontier
                              (filter #(cluster %))
                              (remove #(handled (:pos %)))
                              (sort-by (priority tile))
                              (not-empty))]
            
            (let [cmds (map
                         (fn [target cnt]
                           [:move player cnt pos (:pos target)])
                         targets (distribute (count targets) units))]
              (-> plan
                (update :commands into cmds)
                (update :handled into (map #(nth % 4) cmds))
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
  (let [*side (volatile! nil)
        foe   (opponent player)]
    (reify Algo
      (-move [_ game]
        (when (nil? @*side)
          (vreset! *side (side game player)))
        (let [clusters (clusters game)
              units    (into {}
                         (for [{:keys [pos units owner]} (tile-seq game)
                               :when (= player owner)
                               :when (pos? units)]
                           [pos units]))
              plan     (->
                         {:player   player
                          :foe      foe
                          :scrap    (-> game :scrap player)
                          :game     game
                          :side     @*side
                          :units    units
                          :clusters clusters
                          :handled  #{}
                          :commands []}
                         (build)
                         (defend)
                         (spawn)
                         (spawn-isolated)
                         (spawn-rest)
                         (move))]
          (:commands plan))))))
