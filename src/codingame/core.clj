(ns codingame.core
  (:require
    [codingame.state :refer :all]))

(declare width height)

(defn now []
  (System/currentTimeMillis))

(def t0
  (now))

(defn reset-t! []
  (alter-var-root #'t0 (constantly (now))))

(defn debug [& msg]
  (binding [*out* *err*]
    (apply println (- (now) t0) "ms" msg)))

(defmacro measure [msg & body]
  `(let [t#   (now)
         res# (do ~@body)
         dt#  (- (now) t#)]
     (debug ~msg dt# "ms")
     res#))

(defn zip [& xs]
  (apply map vector xs))

(defn repeatv [n x]
  (vec (repeat n x)))

(defmacro forv [& body]
  `(vec
     (for ~@body)))

(defn distribute [slots units]
  (if (= 0 slots)
    []
    (loop [res   (vec (repeat slots 0))
           i     0
           units units]
      (cond
        (= 0 units) (vec res)
        (>= i (count res)) (recur res 0 units)
        :else (recur
                (update res i inc)
                (inc i)
                (dec units))))))

(defn dist ^long [a b]
  (+ (long (abs (- (:x a) (:x b))))
    (long (abs (- (:y a) (:y b))))))

(defn grass [pos]
  (->Tile pos :neutral 0 0 false))

(defn make-game [w h]
  (->Game
    (forv [y (range h)]
      (forv [x (range w)]
        (->Tile (pos x y) :neutral 0 0 false)))
    w h 0 10 10 5 5))

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
  (for [row  (:grid game)
        tile row]
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
