(ns codingame.algo.mark5
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]
    [clj-async-profiler.core :as profiler]))

;; mark 5

(defn turn0 [{:keys [game player] :as plan}]
  (measure "turn0"
    (let [[cx cy] (:pos
                    (search
                      #(and (= player (:owner %))
                         (= 0 (:units %)))
                      (tile-seq game)))]
      (-> plan
        (update :commands conj [:build player (pos cx cy)])
        (update :building conj (pos cx cy))
        (update :scrap - 10)))))

(defn tile-priority [side]
  (let [side (keyword (name side))]
    (case side
      :left #(- (:x (:pos %)))
      :right #(:x (:pos %)))))

(defn hold-ground [{:keys [player foe units] :as plan}]
  (reduce-kv
    (fn [{:keys [game moving] :as plan} from units]
      (cond+
        (zero? units) plan
        (:dead? from) plan
        :let [targets (->> (neighbour-tiles game from)
                        (filter #(= foe (:owner %)))
                        (filter #(pos? (:units %))))]
        (empty? targets) plan
        :let [enemies (transduce (map :units) + 0 targets)
              stay    (min enemies units)]
        :else
        (-> plan
          (update :moving #(merge-with + % {(:pos from) stay}))
          (update :units update from - stay))))
    plan
    units))

(defn move-closest [{:keys [player foe side units] :as plan}]
  (reduce-kv
    (fn [{:keys [game moving] :as plan} from units]
      (cond+
        (zero? units) plan
        :let [targets  (->> (neighbour-tiles game from)
                         (filter movable?)
                         (remove :dead?)
                         (filter #(not= player (:owner %))))
              targets  (or
                         (not-empty (remove #(moving (:pos %)) targets))
                         (not-empty targets))
              targets  (sort-by*
                         [#(condp = (:owner %)
                             foe      1
                             :neutral 2
                             player   3)
                          (tile-priority side)]
                         targets)]
        (empty? targets) plan
        :let [commands (map (fn [tile cnt]
                              [:move player cnt (:pos from) (:pos tile)])
                         targets (distribute (count targets) units))]
        :else
        (-> plan
          (update :commands into commands)
          (update :moving merge (into {} (map (fn [[_ _ cnt _ tgt]] [tgt cnt]) commands)))
          (update :units dissoc from))))
    plan
    units))

(defn move-far [{:keys [clusters player side units] :as plan}]
  (reduce-kv
    (fn [{:keys [game moving] :as plan} from units]
      (cond+
        (zero? units) plan
        :let [cluster  (search #((:positions %) from) clusters)
              targets  (->> (:positions cluster)
                         (filter #(not= player (:owner %)))
                         (remove #(moving (:pos %)))
                         (sort-by #(dist (:pos from) (:pos %))))]
        (empty? targets) plan
        :let [commands (map (fn [tile cnt]
                              [:move player cnt (:pos from) (:pos tile)])
                         targets (distribute (count targets) units))]
        :else
        (-> plan
          (update :commands into commands)
          (update :moving merge (into {} (map (fn [[_ _ cnt _ tgt]] [tgt cnt]) commands)))
          (update :units dissoc from))))
    plan
    units))

(defn defend [{:keys [game clusters player foe side moving units] :as plan}]
  (let [clashes (->>
                  (for [cluster clusters
                        :when   (and
                                  (pos? (-> cluster :tiles player))
                                  (pos? (-> cluster :tiles foe)))
                        tile    (:positions cluster)
                        :when   (= player (:owner tile))
                        :when   (= 0 (:units tile))
                        :when   (not (:dead? tile))
                        :let    [nbs (->> (neighbour-tiles game tile)
                                       (remove :dead?)
                                       (filter #(= foe (:owner %)))
                                       (filter #(pos? (:units %))))]
                        :when   (pos? (count nbs))]
                    tile)
                  (sort-by (tile-priority side)))]
    (reduce
      (fn [{:keys [scrap] :as plan} {:keys [pos]}]
        (cond+
          (< scrap 10) (reduced plan)
          :else
          (-> plan
            (update :commands conj [:build player pos])
            (update :building conj pos)
            (update :scrap - 10))))
      plan
      clashes)))

(defn spawn-closest [{:keys [game clusters player foe side moving units building] :as plan}]
  (let [frontier (->>
                   (for [cluster clusters
                         :when   (and
                                   (pos? (-> cluster :tiles player))
                                   (pos? (-> cluster :tiles foe)))
                         tile    (:positions cluster)
                         :when   (= player (:owner tile))
                         :when   (not (building (:pos tile)))
                         :when   (not (:dead? tile))
                         :let    [nbs (->> (neighbour-tiles game tile)
                                        (filter movable?)
                                        (remove #(= player (:owner %)))
                                        (remove #(moving (:pos %))))]
                         :when   (pos? (count nbs))]
                     [tile nbs])
                   (sort-by
                     (case side
                       :left
                       (fn [[_ nbs]] (transduce (map #(- (:x (:pos %)))) max Long/MIN_VALUE nbs))
                       :right
                       (fn [[_ nbs]] (transduce (map #(:x (:pos %))) min Long/MAX_VALUE nbs)))))]
    (reduce
      (fn [{:keys [scrap] :as plan} [{:keys [pos]} nbs]]
        (cond+
          (< scrap 10) (reduced plan)
          :let [units (min
                        (quot scrap 10)
                        (+
                          (quot (-> game :scrap foe) 10)
                          (reduce #(+ %1 (max 1 (:units %2))) 0 nbs)
                          1))]
          :else
          (-> plan
            (update :commands conj [:spawn player units pos])
            (update :spawning #(merge-with + % {pos units}))
            (update :scrap - (* 10 units)))))
      plan
      frontier)))

(defn spawn-isolated [{:keys [game scrap clusters player foe side moving units] :as plan}]
  (if (<= scrap 100)
    plan
    (let [frontier (for [cluster clusters
                         :when (and
                                 (pos? (-> cluster :tiles player))
                                 (pos? (+ (-> cluster :tiles foe) (-> cluster :tiles :neutral)))
                                 (zero? (-> cluster :units player)))]
                     (->> (:positions cluster)
                       (filter (fn [tile] (some #(not= player (:owner %)) (neighbour-tiles game tile))))
                       (remove :dead?)
                       (shuffle)
                       (first)))]
      (reduce
        (fn [{:keys [scrap] :as plan} {:keys [pos]}]
          (cond+
            (< scrap 10) (reduced plan)
            :else
            (-> plan
              (update :commands conj [:spawn player 1 pos])
              (update :spawning #(merge-with + % {pos 1}))
              (update :scrap - 10))))
        plan
        frontier))))

(defn units [game player]
  (into {}
    (for [{:keys [units owner] :as tile} (tile-seq game)
          :when (= player owner)
          :when (pos? units)]
      [tile units])))

(defn algo [player]
  (let [*side (volatile! nil)
        foe   (opponent player)]
    (reify Algo
      (-move [_ {:keys [turn] :as game}]
        (reset-t!)
        (debug "Turn" turn)
        (when (= 0 turn)
          (vreset! *side (side game player)))
        (let [plan (as-> {:game     game
                          :player   player
                          :foe      foe
                          :side     @*side
                          :clusters (measure "clusters" (clusters game))
                          :scrap    (-> game :scrap player)
                          :units    (measure "units" (units game player))
                          :building #{}
                          :spawning {}
                          :moving   {}
                          :commands []} %
                     (cond-> % (= 0 turn)      (turn0))
                     (measure "hold-ground"    (hold-ground %))
                     (measure "defend"         (defend %))
                     (measure "move-closest"   (move-closest %))
                     (measure "move-far"       (move-far %))
                     (measure "spawn-closest"  (spawn-closest %))
                     (measure "spawn-isolated" (spawn-isolated %)))]
          (debug (format "  %.3f ms TOTAL" (-> (now) (- t0) (quot 1000) (/ 1000) (double))))
          (:commands plan))))))
