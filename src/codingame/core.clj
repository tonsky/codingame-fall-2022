(ns codingame.core
  (:require
    [codingame.state :refer :all])
  (:import
    [codingame.state Pos]))

;; core

(defn now []
  (System/nanoTime))

(def t0
  (now))

(defn reset-t! []
  (alter-var-root #'t0 (constantly (now))))

(defn debug [& msg]
  (binding [*out* *err*]
    #_(apply println msg)))

(defmacro spy [obj]
  `(let [res# ~obj]
     (debug "spy:" res#)
     res#))

(defmacro measure [msg & body]
  `(let [t#   (now)
         res# (do ~@body)
         dt#  (-> (now) (- t#) (quot 1000) (/ 1000) (double))]
     (debug (format "  %.3f ms" dt#) ~msg)
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

(defmacro vector-some [& xs]
  `(persistent!
     (reduce
       #(if (some? %2)
          (conj! %1 %2)
          %1)
       (transient [])
       [~@xs])))

(defn search [pred xs]
  (some #(when (pred %) %) xs))

(defn sort-by* [fns xs]
  (sort-by
    (fn [x]
      (mapv #(% x) fns))
    xs))

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

(defn dist ^long [^Pos a ^Pos b]
  (+ (long (abs (- (.-x a) (.-x b))))
    (long (abs (- (.-y a) (.-y b))))))

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
                 (map->Tile
                   {:pos       (pos x y)
                    :owner     :neutral 
                    :scrap     0
                    :units     0
                    :units-foe 0
                    :recycler? false
                    :dead?     false})))
     :width  w
     :height h
     :turn   0
     :scrap  {:blue 10 :red 10}}))

(defn get-tile [game [x y]]
  (-> game
    :grid
    (nth y)
    (nth x)))

(defn assoc-tile [game [x y] & kvs]
  (apply update game :grid update y update x assoc kvs))

(defn update-tile [game [x y] f & args]
  (apply update game :grid update y update x f args))

(defn tile-seq [game]
  (for [row   (:grid game)
        tile  row
        :when (pos? (:scrap tile))]
    tile))

(defn inside? [game [x y]]
  (let [{:keys [width height]} game]
    (and
      (<= 0 x)
      (< x width)
      (<= 0 y)
      (< y height))))

(defn when-pred [pred x]
  (when (pred x)
    x))

(defn neighbour-pos [game [x y :as p]]
  (let [{:keys [width height]} game]
    (vector-some
      (when (> x 0)
        (pos (dec x) y))
      (when (< (inc x) width)
        (pos (inc x) y))
      (when (> y 0)
        (pos x (dec y)))
      (when (< (inc y) height)
        (pos x (inc y))))))

(defn neighbour+self-pos [game [x y :as p]]
  (conj (neighbour-pos game [x y]) p))

(defn neighbour-tiles [game [x y :as p]]
  (->> (neighbour-pos game p) 
    (map #(get-tile game %))
    (filterv #(pos? (:scrap %)))))

(defn neighbour+self-tiles [game tile]
  (conj (neighbour-tiles game tile) tile))

(defn recycled?
  ([game tile]
   (and
     (pos? (:scrap tile))
     (some :recycler? (neighbour+self-tiles game tile))))
  ([game tile owner]
   (and
     (pos? (:scrap tile))
     (some 
       #(and (:recycler? %) (= (:owner %) owner))
       (neighbour+self-tiles game tile)))))

(def counter
  (completing
    (fn
      ([] 0)
      ([acc _]
       (inc acc)))))

(defn tiles [game owner]
  (transduce
    (filter #(= (:owner %) owner))
    counter
    0
    (tile-seq game)))

(defn recalc-game [game]
  (measure "recalc-game"
    (let [dead (->> (tile-seq game)
                 (filter #(and (= 1 (:scrap %)) (recycled? game %))))]
      (as-> game %
        (reduce #(assoc-tile %1 %2 :dead? true) % dead)
        (assoc %
          :tiles {:blue (tiles % :blue)
                  :red  (tiles % :red)})))))

(defn flood [game tile]
  (let [^java.util.Collection coll [(:pos tile)]
        queue     (java.util.ArrayDeque. coll)
        reachable (java.util.HashSet. coll)]
    (loop []
      (if (.isEmpty queue)
        (into #{} (map #(get-tile game %)) reachable)
        (let [pos  (.pop queue)
              ns   (into []
                     (comp
                       (remove #(.contains reachable %))
                       (filter #(movable? (get-tile game %))))
                     (neighbour-pos game pos))]
          (.addAll queue ns)
          (.addAll reachable ns)
          (recur))))))

(defn cluster-units [game player cluster]
  (transduce
    (comp
      (filter #(= player (:owner %)))
      (filter #(pos? (:units %)))
      (map :units))
    + 0 cluster))

(defn cluster-tiles [game player cluster]
  (transduce
    (filter #(= player (:owner %)))
    counter 0 cluster))

(defn cluster-info [game cluster]
  {:positions cluster
   :units     {:blue    (cluster-units game :blue cluster)
               :red     (cluster-units game :red cluster)}
   :tiles     {:blue    (cluster-tiles game :blue cluster)
               :red     (cluster-tiles game :red cluster)
               :neutral (cluster-tiles game :neutral cluster)}})

(defn clusters [game]
  (loop [clusters (transient [])
         tiles    (into #{} (filter movable?) (tile-seq game))]
    (if (empty? tiles)
      (persistent! clusters)
      (let [cluster (flood game (first tiles))]
        (recur
          (conj! clusters (cluster-info game cluster))
          (reduce disj tiles cluster))))))

(defn cluster-border [game player cluster]
  (filter
    (fn [tile]
      (and
        (= player (:owner tile))
        (not (:dead? tile))
        (some
          (fn [nb]
            (and
              (pos? (:scrap nb))
              (not= player (:owner nb))))
          (neighbour-tiles game tile))))
    (:positions cluster)))

(defn side [game player]
  (let [fs (->> (tile-seq game)
             (filter #(not= :neutral (:owner %)))
             (filter #(< (:x (:pos %)) (quot (:width game) 2)))
             (map :owner)
             (frequencies))]
    (if (> (get fs player 0) (get fs (opponent player) 0))
      :left
      :right)))
