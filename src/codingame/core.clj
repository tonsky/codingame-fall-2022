(ns codingame.core
  (:require
    [codingame.state :refer :all]))

;; core

(defn now []
  (System/currentTimeMillis))

(def t0
  (now))

(defn reset-t! []
  (alter-var-root #'t0 (constantly (now))))

(defn debug [& msg]
  (binding [*out* *err*]
    (apply println (- (now) t0) "ms" msg)))

(defmacro spy [obj]
  `(let [res# ~obj]
     (debug "spy:" res#)
     res#))

(defmacro measure [msg & body]
  `(let [t#   (now)
         res# (do ~@body)
         dt#  (- (now) t#)]
     (debug ~msg dt# "ms")
     res#))

(defmacro cond+ [& clauses]
  (when-some [[test expr & rest] clauses]
    (condp = test
      :do   `(do ~expr (cond+ ~@rest))
      :let  `(let ~expr (cond+ ~@rest))
      :some `(or ~expr (cond+ ~@rest))
      `(if ~test ~expr (cond+ ~@rest)))))

(defn zip [& xs]
  (apply map vector xs))

(defn repeatv [n x]
  (vec (repeat n x)))

(defmacro forv [& body]
  `(vec
     (for ~@body)))

(defn search [pred xs]
  (some #(when (pred %) %) xs))

(defn distribute [slots units]
  (if (= 0 slots)
    []
    (loop [res   (vec (repeat slots 0))
           i     0
           units units]
      (cond
        (= 0 units) (take-while pos? res)
        (>= i (count res)) (recur res 0 units)
        :else (recur
                (update res i inc)
                (inc i)
                (dec units))))))

(defn dist ^long [a b]
  (+ (long (abs (- (:x a) (:x b))))
    (long (abs (- (:y a) (:y b))))))

(defn grass [pos]
  (map->Tile
    {:pos       pos
     :owner     :neutral 
     :scrap     0
     :units     0
     :units-foe 0
     :recycler? false
     :dead?     false}))

(defn opponent [player]
  (case player
    :blue :red
    :red :blue))

(defn movable? [tile]
  (and
    (pos? (:scrap tile))
    (not (:recycler? tile))))

(defn make-game [w h]
  (map->Game
    {:grid   (forv [y (range h)]
               (forv [x (range w)]
                 (grass (pos x y))))
     :width  w
     :height h
     :turn   0
     :scrap  {:blue 10 :red 10}}))

(defn get-tile [game pos]
  (-> game
    :grid
    (nth (:y pos))
    (nth (:x pos))))

(defn set-tile [game pos val]
  (update game :grid update (:y pos) assoc (:x pos) val))

(defn assoc-tile [game pos & kvs]
  (apply update game :grid update (:y pos) update (:x pos) assoc kvs))

(defn update-tile [game pos f & args]
  (apply update game :grid update (:y pos) update (:x pos) f args))

(defn tile-seq [game]
  (for [row   (:grid game)
        tile  row
        :when (pos? (:scrap tile))]
    tile))

(defn pos-seq [game]
  (map :pos (tile-seq game)))

(defn inside? [game pos]
  (let [{:keys [width height]} game
        {:keys [x y]} pos]
    (and
      (<= 0 x)
      (< x width)
      (<= 0 y)
      (< y height))))

(defn neighbours [game [x y]]
  (let [{:keys [width height]} game]
    (filterv some?
      [(when (> x 0)
         (pos (dec x) y))
       (when (< (inc x) width)
         (pos (inc x) y))
       (when (> y 0)
         (pos x (dec y)))
       (when (< (inc y) height)
         (pos x (inc y)))])))

(defn neighbours+self [game pos]
  (cons pos (neighbours game pos)))

(defn recycled?
  ([game pos]
   (let [tile (get-tile game pos)]
     (and
       (pos? (:scrap tile))
       (some #(:recycler? (get-tile game %)) (neighbours+self game pos)))))
  ([game pos owner]
   (let [tile (get-tile game pos)]
     (and
       (pos? (:scrap tile))
       (some #(let [tile (get-tile game %)]
                (and (:recycler? tile) (= (:owner tile) owner))) (neighbours+self game pos))))))

(defn tiles [game owner]
  (count (filter #(= (:owner %) owner) (tile-seq game))))

(defn recalc-game [game]
  (let [dead (->> (tile-seq game)
               (filter #(and (= 1 (:scrap %))
                          (recycled? game (:pos %)))))]
    (as-> game %
      (reduce
        #(assoc-tile %1 (:pos %2) :dead? true)
        % dead)
      (assoc %
        :tiles {:blue (tiles % :blue)
                :red  (tiles % :red)}))))

(defn flood [game pos]
  (loop [reachable #{pos}
         queue     (conj clojure.lang.PersistentQueue/EMPTY
                     pos)]
    (if (empty? queue)
      reachable
      (let [pos (peek queue)
            ns  (->> (neighbours game pos)
                  (filter #(movable? (get-tile game %)))
                  (remove reachable))]
        (recur
          (into reachable ns)
          (into (pop queue) ns))))))

(defn cluster-units [game player cluster] 
  (->> cluster
    (map #(get-tile game %))
    (filter #(= player (:owner %)))
    (filter #(pos? (:units %)))
    (map :units)
    (reduce + 0)))

(defn cluster-tiles [game player cluster] 
  (->> cluster
    (map #(get-tile game %))
    (filter #(= player (:owner %)))
    (count)))

(defn cluster-info [game cluster]
  {:positions cluster
   :units     {:blue    (cluster-units game :blue cluster)
               :red     (cluster-units game :red cluster)}
   :tiles     {:blue    (cluster-tiles game :blue cluster)
               :red     (cluster-tiles game :red cluster)
               :neutral (cluster-tiles game :neutral cluster)}})

(defn clusters [game]
  (loop [clusters  #{}
         positions (->> (pos-seq game)
                     (filter #(movable? (get-tile game %)))
                     set)]
    (if (empty? positions)
      clusters
      (let [cluster (flood game (first positions))]
        (recur
          (conj clusters (cluster-info game cluster))
          (reduce disj positions cluster))))))

(defn cluster-border [game player cluster]
  (->> (:positions cluster)
    (filter
      (fn [pos]
        (let [tile (get-tile game pos)]
          (and
            (= player (:owner tile))
            (not (:dead? tile))
            (some
              (fn [nb]
                (let [tile (get-tile game nb)]
                  (and
                    (pos? (:scrap tile))
                    (not= player (:owner tile)))))
              (neighbours game pos))))))))
