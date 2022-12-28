(ns codingame.algo.mark5
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

;; mark 5

(defn defend [{:keys [player foe game clusters] :as plan}]
  (let [need-def (->>
                   (for [tile (tile-seq game)
                         :when (= player (:owner tile))
                         :when (not (:recycler? tile))
                         :let  [ns  (neighbours game (:pos tile))
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

(defn income [game player]
  (->>
    (for [tile  (tile-seq game)
          :when (:recycler? tile)
          :when (= player (:owner tile))
          nb    (neighbours+self game (:pos tile))
          :when (pos? (:scrap (get-tile game nb)))]
      nb)
    set
    count))

(defn build [{:keys [player foe side game clusters] :as plan}]
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
        income-me     (income game player)
        income-foe    (income game foe)
        buildable     (tile-seq game)
        buildable     (if (> income-foe income-me)
                        (filter
                          #(case side
                             :left  (>= (:x (:pos %)) (- (quot (:width game) 2) 1))
                             :right (<= (:x (:pos %)) (+ (quot (:width game) 2) 1)))
                          buildable)
                        (filter
                          #(case side
                             :left  (>= (:x (:pos %)) (+ (quot (:width game) 2) 1))
                             :right (<= (:x (:pos %)) (- (quot (:width game) 2) 1)))
                          buildable))
        buildable     (->> buildable
                        (filter
                          #(and
                             (= player (:owner %))
                             (pos? (:scrap %))
                             (zero? (:units %))
                             (not (:recycler? %))
                             (not (:dead? %))))
                        (remove
                          (fn [tile]
                            (some #(contains? % (:pos tile)) my-clusters)))
                        (sort-by #(- (priority-pos %))))]
    (reduce
      (fn [{:keys [game handled scrap] :as plan} {:keys [pos]}]
        (cond
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          ;; don’t build next to recycler
          (some #(:recycler? (get-tile game %)) (neighbours game pos)) plan
          ;; don’t build next to recycled?
          ; (some #(recycled? game %) (neighbours game pos)) plan
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

(defn move [{:keys [player foe side game units clusters handled] :as plan}]
  (let [priority (fn [pos]
                   (fn [target]
                     (let [{:keys [owner units]} (get-tile game target)]
                       (+
                         (* 20 (dist pos target))
                         (* 10 (cond
                                 (and (= foe owner) (pos? units))  1
                                 (= :neutral owner)                2
                                 (and (= foe owner) (zero? units)) 3
                                 :else                             4))
                         (* 100 (case [side (> (:x pos) (:x target))]
                                 [:left true]   2
                                 [:left false]  1
                                 [:right true]  1
                                 [:right false] 2))))))
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
      (fn [{:keys [handled] :as plan} pos units]
        (let [cluster (search #(contains? % pos) (map :positions clusters))]
          (if-some [targets (->> frontier
                              (filter #(cluster %))
                              (remove handled)
                              (sort-by (priority pos))
                              (not-empty))]
            (let [cmds (map
                         (fn [target cnt]
                           [:move player cnt pos target])
                         targets (distribute (count targets) units))]
              (-> plan
                (update :commands into cmds)
                ; (update :handled into (map #(nth % 4) cmds))
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

(defn side [game player]
  (let [fs (->> (tile-seq game)
             (filter #(< (:x (:pos %)) (quot (:width game) 2)))
             (map :owner)
             (frequencies))]
    (if (> (get fs player 0) (get fs (opponent player) 0))
      :left
      :right)))

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
                         (move)
                         (build)
                         (defend)
                         (spawn)
                         (spawn-isolated)
                         (spawn-rest))]
          (:commands plan))))))
