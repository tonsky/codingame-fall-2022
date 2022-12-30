(ns codingame.algo.mark2
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]))

(defn build [{:keys [player foe game units clusters] :as plan}]
  (let [priority-tile (fn [pos]
                        (let [{:keys [scrap]} (get-tile game pos)]
                          (* scrap
                            (cond
                              (recycled? game pos foe)   3
                              (not (recycled? game pos)) 1
                              :else                      0))))
        priority-pos  (fn [tgt]
                        (transduce (map priority-tile) + 0 (neighbour+self-pos game (:pos tgt))))
        my-clusters   (->> clusters
                        (filter #(zero? (-> % :tiles foe)))
                        (map :positions))
        middle        (->> (tile-seq game)
                        (filter #(<= (quot (:width game) 3) (:x (:pos %)) (* 2 (quot (:width game) 3))))
                        (filter #(and
                                   (= player (:owner %))
                                   (pos? (:scrap %))
                                   (zero? (:units %))
                                   (not (:recycler? %))))
                        (remove (fn [tile] (some #(contains? % (:pos tile)) my-clusters)))
                        (sort-by #(- (priority-pos %))))]
    (reduce
      (fn [{:keys [game handled scrap] :as plan} {:keys [pos] :as tile}]
        (cond
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          ;; don’t build next to recycler
          (some #(:recycler? %) (neighbour-tiles game pos)) plan
          :else
          (-> plan
            (update :game assoc-tile pos :recycler? true)
            (update :handled conj pos)
            (update :commands conj [:build player pos])
            (update :scrap - 10))))
      plan
      middle)))

(defn spawn [{:keys [player foe game units clusters frontier] :as plan}]
  (let [my-clusters (filter
                      (fn [cluster]
                        (and 
                          ;; less units than foe
                          (<=
                            (-> cluster :units player)
                            (-> cluster :units foe))
                          ;; can build, but not everything is mine
                          (< 0 (-> cluster :tiles player) (-> cluster :positions count))))
                      clusters)
        find-spawn (fn [cluster]
                     (rand-nth
                       (for [pos       (:positions cluster)
                             :when     (= player (:owner (get-tile game pos)))
                             neighbour (neighbour-pos game pos)
                             :when     (not= player (:owner (get-tile game neighbour)))]
                         pos)))]
    (reduce
      (fn [{:keys [game handled scrap] :as plan} cluster]
        (cond+
          (handled pos) plan
          (< scrap 10)  (reduced plan)
          :let [pos (find-spawn cluster)]
          (nil? pos)    plan
          :let [cnt (min
                      (quot scrap 10)
                      (-> cluster :units foe (+ 1)))]
          :else
          (-> plan
            (update :commands conj [:spawn player cnt pos])
            (update :handled conj pos)
            (update :scrap - (* cnt 10)))))
      plan
      my-clusters)))

(defn move [{:keys [player foe game units clusters frontier] :as plan}]
  (let [center   (pos (quot (:width game) 2) (quot (:height game) 2))
        priority (fn [pos]
                   (fn [target]
                     (let [{:keys [owner units] :as tile} (get-tile game target)]
                       (+
                         (* 20 (dist pos target))
                         (* 10 (cond
                                 (and (= foe owner) (zero? units)) 1
                                 (= :neutral owner) 2
                                 (= foe owner)      3
                                 :else              4))
                         (* 1 (dist center target))))))]
    (reduce-kv
      (fn [plan pos units]
        (let [cluster (search #(contains? % pos) (map :positions clusters))]
          (if-some [target (->> frontier
                             (sort-by (priority pos))
                             (search #(and (cluster %) (not ((:handled plan) %)))))]
            (-> plan
              (update :handled conj target)
              (update :commands conj [:move player units pos target])
              (update :units dissoc pos))
            plan)))
      plan
      units)))

(defn algo [player]
  (reify Algo
    (-move [_ game]
      (let [clusters (clusters game)
            units    (into {}
                       (for [{:keys [pos units owner]} (tile-seq game)
                             :when (= player owner)
                             :when (pos? units)]
                         [pos units]))
            frontier (set
                       (for [tile      (tile-seq game)
                             :when     (= player (:owner tile))
                             neighbour (neighbour-pos game (:pos tile))
                             :let      [neighbour (get-tile game neighbour)]
                             :when     (movable? neighbour)
                             :when     (not= player (:owner neighbour))
                             ;; don’t go to tile that will be removed
                             :when     (or (not (recycled? game (:pos neighbour)))
                                         (> (:scrap neighbour) 1))]
                         (:pos neighbour)))
            foe      (opponent player)
            plan     (->
                       {:player   player
                        :foe      foe
                        :scrap    (-> game :scrap player)
                        :game     game
                        :units    units
                        :clusters clusters
                        :frontier frontier
                        :handled  #{}
                        :commands []}
                       (build)
                       (spawn)
                       (move))]
        (:commands plan)))))
