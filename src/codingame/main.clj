(ns codingame.main
  (:require
    [codingame.core :refer :all]
    [codingame.state :refer :all]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.window :as window]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.skija Color FontStyle Paint Typeface]))

; 136 ms scrap ([0 26] [1 2] [4 48] [6 14] [8 76] [9 24] [10 20])

; 8  8  9  4  0  8  4  9  8  10  4  4  4  0  4  9  8  8  8  8  4
; 0  8  0  8  4  0  8  8  9  10  8  8  8  0  0  6  8  0  8  9  8
; 0  6  4  8  8  8  10  4  4  9  4  8  10  8  9  4  4  1  10  6  4
; 4  9  8  9  4  4  8  9  4  10  10  8  8  10  4  0  8  10  6  4  9
; 8  8  4  8  0  8  6  8  0  0  8  8  8  6  10  4  8  8  6  4  9
; 9  4  6  8  8  4  10  6  8  8  8  0  0  8  6  8  0  8  4  8  8
; 9  4  6  10  8  0  4  10  8  8  10  10  4  9  8  4  4  9  8  9  4
; 4  6  10  1  4  4  9  8  10  8  4  9  4  4  10  8  8  8  4  6  0
; 8  9  8  0  8  6  0  0  8  8  8  10  9  8  8  0  4  8  0  8  0
; 4  8  8  8  8  9  4  0  4  4  4  10  8  9  4  8  0  4  9  8  8

(defn sample-game []
  (let [w 21
        h 10
        game  (make-game w h)
        scrap (->> (concat
                     (repeat 28 0)
                     (repeat 48 4)
                     (repeat 14 6)
                     (repeat 76 8)
                     (repeat 24 9)
                     (repeat 20 10))
                shuffle)
        game (reduce
               (fn [game [pos scrap]]
                 (assoc-tile game pos :scrap scrap))
               game
               (zip (for [y (range h) x (range w)] (pos x y)) scrap))
        blue-pos (pos 4 3)
        red-pos  (pos 17 3)
        game (as-> game %
               (assoc-tile % blue-pos
                 :owner     :blue
                 :scrap     8
                 :recycler? true)
               (reduce
                 (fn [game pos]
                   (assoc-tile game pos
                     :owner :blue
                     :scrap 8
                     :units 1))
                 % (neighbours % blue-pos))
               (assoc-tile % red-pos
                 :owner :red
                 :scrap 8)
               (reduce
                 (fn [game pos]
                   (assoc-tile game pos
                     :owner :red
                     :scrap 8
                     :units 1))
                 % (neighbours % red-pos)))]
    game))

(defn proceed [game]
  (let [{:keys [grid scrap-blue scrap-red]} game
        recycled-blue (filter #(recycled? game % :blue) (pos-seq game))
        recycled-red  (filter #(recycled? game % :red) (pos-seq game))]
    (as-> game %
      (update % :turn inc)
      (update % :scrap-blue + 10 (count recycled-blue))
      (update % :scrap-red + 10 (count recycled-red))
      (reduce
        (fn [game pos]
          (let [tile  (get-tile game pos)
                tile' (if (> (:scrap tile) 1)
                        (update tile :scrap dec)
                        (grass pos))]
            (set-tile game pos tile')))
        %
        (set (concat recycled-blue recycled-red))))))

(defn reset-game []
  (let [turn0 (sample-game)
        turns (vec (take 100 (iterate proceed turn0)))]
    (reset! *games turns)
    (swap! *turn assoc
      :value 0
      :min   0
      :max   (dec (count turns)))))

(defn current-game []
  (nth @*games (:value @*turn)))

(defn tile-size [game cs]
  (min
    (quot (:width cs) (:width game))
    (quot (:height cs) (:height game))))

(def face-ui
  (Typeface/makeFromName "Case Micro" FontStyle/NORMAL))

(def fill-blue
  (paint/fill 0xFF0033CC))

(def fill-red
  (paint/fill 0xFFCC3300))

(def color-scrap
  (unchecked-int 0xFFB1AA98))

(def color-scrap-blue
  (unchecked-int 0xFF3B68CF))

(def color-scrap-red
  (unchecked-int 0xFFB34229))

(def fill-unit
  (paint/fill 0xFFFFFFFF))

(def icon-unit
  (ui/svg "resources/unit.svg"))

(def icon-recycler
  (ui/svg "resources/recycler.svg"))

(def icon-recycled
  (ui/svg "resources/recycled.svg"))

(def fill-text
  (paint/fill 0xFFFFFFFF))

(defn on-paint [ctx canvas size]
  (let [{:keys [grid width height turn scrap-blue scrap-red] :as game} (current-game)
        {:keys [font-ui scale]} ctx
        tile-size (tile-size game size)]
    (with-open [fill (paint/fill 0xFF000000)]
      (doseq [tile (tile-seq game)
              :let [{:keys [pos owner scrap units recycler?]} tile]
              :when (> scrap 0)
              :let [left  (* tile-size (:x pos))
                    top   (* tile-size (:y pos))
                    rect  (core/irect-xywh left top (- tile-size 2) (- tile-size 2))
                    alpha 255]]
        (.setColor fill
          (cond
            (= :neutral owner) (Color/withA color-scrap alpha)
            (= :blue owner)    (Color/withA color-scrap-blue alpha)
            (= :red owner)     (Color/withA color-scrap-red alpha)))
        (canvas/draw-rect canvas rect fill)

        (doseq [x (range 0 scrap)]
          (canvas/draw-rect canvas (core/rect-xywh (+ left 4 (* x 8)) (+ top 4) 6 6) fill-text))
        ; (canvas/draw-string canvas (str scrap) (+ left (* 1 scale)) (+ top (* 12 scale)) font-ui fill-text)

        (when (pos? units)
          (core/draw icon-unit ctx rect canvas)
          (canvas/draw-string canvas (str units) (+ left (* 2 scale)) (+ top tile-size (- (* 3 scale))) font-ui fill-text))

        (when (recycled? game pos)
          (core/draw icon-recycled ctx rect canvas))

        (when recycler?
          (core/draw icon-recycler ctx rect canvas))

        ))))

(defn stats [& kvs]
  (ui/grid
    (for [[k v] (partition 2 kvs)]
      [(ui/padding 0 0 10 15
         (ui/label (str k)))
       (ui/padding 0 0 0 15
         (ui/max-width
           [(ui/label {:features ["tnum"]} "1000")]
           (ui/halign 1
             (ui/label {:features ["tnum"]} (str v)))))])))

(def app
  (ui/default-theme {:face-ui face-ui}
    (ui/focus-controller
      (ui/valign 0.5
        (ui/row
          [:stretch 1
           (ui/padding 10 0 20 0
             (ui/with-bounds ::bounds
               (ui/dynamic ctx [game      (current-game)
                                tile-size (tile-size game (::bounds ctx))]
                 (ui/width (* tile-size (:width game))
                   (ui/height (* tile-size (:height game))
                     (ui/canvas {:on-paint on-paint}))))))]
          (ui/padding 0 0 20 0
            (ui/column
              (ui/dynamic _ [game (current-game)]
                (stats
                  "Tiles blue" (tiles game :blue)
                  "Tiles red"  (tiles game :red)
                  "Scrap blue" (:scrap-blue game)
                  "Scrap red"  (:scrap-red game)
                  "Turn"       (:turn game)))
              (ui/gap 0 15)
              (ui/row
                [:stretch 1
                 (ui/button
                   (fn [] (swap! *turn update :value #(if (pos? %) (dec %) %)))
                   (ui/label "←"))]
                (ui/gap 15 0)
                [:stretch 1
                 (ui/button
                   (fn []
                     (swap! *turn
                       #(cond-> %
                          (< (:value %) (:max %))
                          (update :value inc))))
                   (ui/label "→"))])
              (ui/gap 0 15)
              (ui/slider *turn)
              (ui/gap 0 15)
              (ui/button
                #(reset-game)
                (ui/label "Restart")))))))))

(defn -main [& args]
  (reset-game)
  (ui/start-app!
    (reset! *window
      (ui/window
        {:title "Coding Games Fall Challenge 2022"}
        #'app)))
  (apply nrepl/-main args))

(comment
  (reset-game)
  @*games
  @*turn
  (-main))