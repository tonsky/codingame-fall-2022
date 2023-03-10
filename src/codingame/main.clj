(ns codingame.main
  (:require
    [clojure.data.priority-map :as priority-map]
    [clojure.string :as str]
    [codingame.core :refer :all]
    [codingame.state :refer :all]
    [codingame.algo.random :as algo.random]
    [codingame.algo.mark1 :as algo.mark1]
    [codingame.algo.mark2 :as algo.mark2]
    [codingame.algo.mark3 :as algo.mark3]
    [codingame.algo.mark4 :as algo.mark4]
    [codingame.algo.mark5 :as algo.mark5]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.canvas :as canvas]
    [io.github.humbleui.core :as core]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.window :as window]
    [nrepl.cmdline :as nrepl])
  (:import
    [io.github.humbleui.skija Color FontStyle Paint Typeface]))

(defn sample-game []
  (let [w        (+ 12 (rand-int 12))
        h        (quot w 2)
        game     (make-game w h)
        game     (reduce
                   (fn [game pos]
                     (assoc-tile game pos :scrap (rand-nth [10 9 8 8 8 6 4 4 0])))
                   game
                   (for [y (range h)
                         x (range w)]
                     (pos x y)))
        [bx by] [(+ 1 (rand-int (quot w 4))) (+ 1 (rand-int (- h 2)))]
        [rx ry] [(- w 1 bx) (if (< (rand) 0.5) by (- h 1 by))]]
    (-> game
      (assoc-tile [bx by] :owner :blue :scrap 8)
      (assoc-tile [(dec bx) by] :owner :blue :scrap 8 :units 1)
      (assoc-tile [(inc bx) by] :owner :blue :scrap 8 :units 1)
      (assoc-tile [bx (dec by)] :owner :blue :scrap 8 :units 1)
      (assoc-tile [bx (inc by)] :owner :blue :scrap 8 :units 1)
      (assoc-tile [rx ry] :owner :red :scrap 8)
      (assoc-tile [(dec rx) ry] :owner :red :scrap 8 :units 1)
      (assoc-tile [(inc rx) ry] :owner :red :scrap 8 :units 1)
      (assoc-tile [rx (dec ry)] :owner :red :scrap 8 :units 1)
      (assoc-tile [rx (inc ry)] :owner :red :scrap 8 :units 1)
      (recalc-game))))

(defn warn [game & msg]
  #_(apply println "WARN turn" (:turn game) msg))

(defmulti exec
  (fn [game cmd]
    (first cmd)))

(defmethod exec :build [game [_ player pos :as cmd]]
  (let [tile (get-tile game pos)]
    (cond+
      (not= player (:owner tile))
      (do (warn game "Can't" cmd "at other's tile" tile) game)
      
      (<= (:scrap tile) 0)
      (do (warn game "Can't" cmd "at grass" tile) game)
      
      (:recycler? tile)
      (do (warn game "Can't" cmd "at recycler" tile) game)
      
      :let [scrap (-> game :scrap player)]
      (< scrap 10)
      (do (warn game "Not enough scrap" scrap "to" cmd) game)
      
      :else
      (-> game
        (assoc-tile pos
          :units     0
          :units-foe 0
          :recycler? true)
        (update :scrap update player - 10)))))

(defn penultimate [xs]
  (loop [a  (first xs)
         b  (fnext xs)
         xs (nnext xs)]
    (if (empty? xs)
      a
      (recur b (first xs) (next xs)))))

(defn find-path [game from to]
  (loop [dists {from 0}
         prevs {}
         queue (priority-map/priority-map
                 from (dist from to))]
    (cond+
      (empty? queue)
      nil
      
      :let [[pos _] (peek queue)]
      
      (= pos to)
      (->> (iterate prevs to)
        (take-while some?)
        (penultimate))
      
      :let [dist' (inc (dists pos))
            [dists' prevs' queue']
            (reduce
              (fn [[dists prevs queue] pos']
                (let [tile' (get-tile game pos')]
                  (if (or
                        (= 0 (:scrap tile'))
                        (:recycler? tile')
                        (<= (dists pos' Long/MAX_VALUE) dist'))
                    [dists prevs queue]
                    [(assoc dists pos' dist')
                     (assoc prevs pos' pos)
                     (assoc queue pos' (+ dist' (dist pos' to)))])))
              [dists prevs (pop queue)]
              (neighbour-pos game pos))]
      :else
      (recur dists' prevs' queue'))))

(defmethod exec :move [game [_ player units pos-from pos-to :as cmd]]
  (if-some [pos-to' (find-path game pos-from pos-to)]
    (let [from (get-tile game pos-from)
          to (get-tile game pos-to')]
      (cond+
        (not= player (:owner from))
        (do (warn game "Can't" cmd "other's units" from) game)
    
        (< (:units from) units)
        (do (warn game "Not enough units to" cmd "from" from) game)
    
        (:recycler? to)
        (do (warn game "Can't" cmd "to recycler" to) game)
    
        (= 0 (:scrap to))
        (do (warn game "Can't" cmd "to grass" to) game)
    
        (= :neutral (:owner to))
        (-> game
          (update-tile (:pos from) update :units - units)
          (update-tile (:pos to) update :units + units)
          (assoc-tile (:pos to)
            :owner player))
    
        (= player (:owner to))
        (-> game
          (update-tile (:pos from) update :units - units)
          (update-tile (:pos to) update :units + units))
    
        :else
        (-> game
          (update-tile (:pos from) update :units - units)
          (update-tile (:pos to) update :units-foe + units))))
    (do (warn game "Not reachable" cmd) game)))

(defmethod exec :spawn [game [_ player units pos :as cmd]]
  (let [tile (get-tile game pos)]
    (cond+
      (not= player (:owner tile))
      (do (warn game "Can't" cmd "at other's tile" tile) game)
      
      (<= (:scrap tile) 0)
      (do (warn game "Can't" cmd "at grass" tile) game)
      
      (:recycler? tile)
      (do (warn game "Can't" cmd "at recycler" tile) game)
      
      :let [scrap (-> game :scrap player)]
      (< scrap (* 10 units))
      (do (warn game "Not enough scrap" scrap "to" cmd) game)
      
      :else
      (-> game
        (update-tile pos update :units + units)
        (update :scrap update player - (* 10 units))))))

(defn update-owners [game]
  (reduce
    (fn [game tile]
      (let [{:keys [pos owner units units-foe]} tile]
        (cond
          (= 0 units-foe)
          game
          
          (<= units-foe units)
          (assoc-tile game pos
            :units (- units units-foe)
            :units-foe 0)
          
          :else
          (assoc-tile game pos
            :units (- units-foe units)
            :units-foe 0
            :owner (opponent owner)))))
    game
    (tile-seq game)))

(defn priority-fn [[cmd player & args]]
  [({:move  0
     :build 1
     :spawn 2} cmd)
   ({:blue 0
     :red  1} player)])

(defn proceed [game moves]
  (let [{:keys [grid scrap]} game
        moves         (sort-by priority-fn moves)
        recycled-blue (filter #(recycled? game % :blue) (tile-seq game))
        recycled-red  (filter #(recycled? game % :red)  (tile-seq game))]
    (as-> game %
      (reduce exec % moves)
      (update-owners %)
      (update % :turn inc)
      (update % :scrap update :blue + 10 (count recycled-blue))
      (update % :scrap update :red + 10 (count recycled-red))
      (reduce
        (fn [game tile]
          (if (> (:scrap tile) 1)
            (update-tile game tile update :scrap dec)
            (assoc-tile game tile
              :owner     :neutral 
              :scrap     0
              :units     0
              :units-foe 0
              :recycler? false)))
        %
        (set (concat recycled-blue recycled-red)))
      (recalc-game %))))

(defn reset-game []
  (let [algo-blue (algo.mark5/algo :blue)
        algo-red  (algo.mark4/algo :red)]
    (loop [games [(sample-game)]
           moves []]
      (if (> (count games) 100)
        (do
          (reset! *games games)
          (reset! *moves moves)
          (swap! *turn assoc
            :value 0
            :min   0
            :max   (dec (count moves))))
        (let [game       (peek games)
              moves-blue (-move algo-blue game)
              moves-red  (-move algo-red game)
              moves'     (concat moves-blue moves-red)
              game'      (proceed game moves')]
          (recur (conj games game') (conj moves moves'))))))
  (redraw))

(defn current-game []
  (nth @*games (:value @*turn)))

(defn current-moves []
  (nth @*moves (:value @*turn)))

(defn tile-size [game cs]
  (min
    (quot (:width cs) (:width game))
    (quot (:height cs) (:height game))))

(def face-ui
  (Typeface/makeFromName "Case Micro" FontStyle/NORMAL))

(def fill-grass
  (paint/fill 0xFFE8E8E8))

(def fill-scrap-neutral
  (paint/fill 0xFFAAAAAA))

(def fill-scrap-blue
  (paint/fill 0xFF3B68CF))

(def fill-scrap-red
  (paint/fill 0xFFB34229))

(def icon-unit-blue
  (ui/svg "resources/unit_blue.svg"))

(def icon-unit-red
  (ui/svg "resources/unit_red.svg"))

(def icon-recycler
  (ui/svg "resources/recycler.svg"))

(def icon-recycled
  (ui/svg "resources/recycled.svg"))

(def icon-build
  (ui/svg "resources/build.svg"))

(def icon-spawn
  (ui/svg "resources/spawn.svg"))

(def fill-text
  (paint/fill 0xFFFFFFFF))

(def stroke-move
  (paint/stroke 0x80FFFFFF 6))

(defn on-paint [ctx canvas size]
  (let [{:keys [font-ui scale]} ctx
        {:keys [grid width height turn] :as game} (current-game)
        moves (current-moves)
        tile-size (tile-size game size)]
    (doseq [tile (tile-seq game)
            :let [{:keys [pos owner scrap units recycler?]} tile
                  left  (* tile-size (:x pos))
                  top   (* tile-size (:y pos))
                  rect  (core/irect-xywh left top (- tile-size 2) (- tile-size 2))]]
      (canvas/draw-rect canvas rect
        (cond
          (= 0 scrap)        fill-grass
          (= :neutral owner) fill-scrap-neutral
          (= :blue owner)    fill-scrap-blue
          (= :red owner)     fill-scrap-red))

      (doseq [x (range 0 scrap)]
        (canvas/draw-rect canvas (core/rect-xywh (+ left 4 (* x 8)) (+ top 4) 6 6) fill-text)))
    
    (doseq [tile (tile-seq game)
            :let [{:keys [pos owner scrap units recycler?]} tile
                  left  (* tile-size (:x pos))
                  top   (* tile-size (:y pos))
                  rect  (core/irect-xywh left top (- tile-size 2) (- tile-size 2))]]
      (when (pos? units)
        (core/draw (case owner :blue icon-unit-blue :red icon-unit-red) ctx rect canvas)
        (canvas/draw-string canvas (str units) (+ left (* 2 scale)) (+ top tile-size (- (* 3 scale))) font-ui fill-text))

      (when recycler?
        (core/draw icon-recycler ctx rect canvas))
    
      ; (when (recycled? game tile)
        ; (core/draw icon-recycled ctx rect canvas))
      )
    
    (doseq [[cmd player & rest] moves]
      (case cmd
        :move
        (let [[units from to] rest]
          (canvas/draw-line canvas
            (core/point
              (-> (:x from) (+ 0.5) (* tile-size))
              (-> (:y from) (+ 0.5) (* tile-size)))
            (core/point
              (-> (:x to) (+ 0.5) (* tile-size))
              (-> (:y to) (+ 0.5) (* tile-size)))
            stroke-move))
        
        :build
        (let [[pos] rest]
          (core/draw icon-build ctx (core/irect-xywh (* (:x pos) tile-size) (* (:y pos) tile-size) tile-size tile-size) canvas))
        
        :spawn
        (let [[units pos] rest
              tile (get-tile game pos)]
          ; (when (and
          ;         (= 0 (:units tile))
          ;         (not (:recycler? tile)))
          (core/draw icon-spawn ctx (core/irect-xywh (* (:x pos) tile-size) (* (:y pos) tile-size) tile-size tile-size) canvas))
          
        nil))))

(def stroke-graph-blue
  (paint/stroke 0x800033CC 4))

(def stroke-graph-red
  (paint/stroke 0x80CC3300 4))

(defn paint-tiles [ctx canvas size]
  (canvas/clear canvas 0xFFFFFFFF)
  (let [step   (/ (:x size) (inc (:max @*turn)))
        vscale (/ (:y size)
                 (reduce max 0
                   (concat
                     (map #(-> % :tiles :blue) @*games)
                     (map #(-> % :tiles :red) @*games))))]
    (doseq [[i g1 g2] (zip (range (inc (:max @*turn))) @*games (next @*games))]
      (canvas/draw-line canvas
        (core/ipoint (* i step)       (- (:y size) (* vscale (-> g1 :tiles :blue))))
        (core/ipoint (* (inc i) step) (- (:y size) (* vscale (-> g2 :tiles :blue))))
        stroke-graph-blue)
      (canvas/draw-line canvas
        (core/ipoint (* i step)       (- (:y size) (* vscale (-> g1 :tiles :red))))
        (core/ipoint (* (inc i) step) (- (:y size) (* vscale (-> g2 :tiles :red))))
        stroke-graph-red))))

(defn paint-scrap [ctx canvas size]
  (canvas/clear canvas 0xFFFFFFFF)
  (let [step   (/ (:x size) (inc (:max @*turn)))
        vscale (/ (:y size)
                 (reduce max 0
                   (concat 
                     (map #(-> % :scrap :blue) @*games) 
                     (map #(-> % :scrap :red) @*games))))]
    (doseq [[i g1 g2] (zip (range (inc (:max @*turn))) @*games (next @*games))]
      (canvas/draw-line canvas
        (core/ipoint (* i step)       (- (:y size) (* vscale (-> g1 :scrap :blue))))
        (core/ipoint (* (inc i) step) (- (:y size) (* vscale (-> g2 :scrap :blue))))
        stroke-graph-blue)
      (canvas/draw-line canvas
        (core/ipoint (* i step)       (- (:y size) (* vscale (-> g1 :scrap :red))))
        (core/ipoint (* (inc i) step) (- (:y size) (* vscale (-> g2 :scrap :red))))
        stroke-graph-red))))

(defn prev-turn! []
  (swap! *turn update :value #(if (pos? %) (dec %) %))
  (redraw)
  true)

(defn next-turn! []
  (swap! *turn #(cond-> % (< (:value %) (:max %)) (update :value inc)))
  (redraw)
  true)

(def app
  (ui/default-theme {:face-ui face-ui}
    (ui/focus-controller
      (ui/event-listener :key
        (fn [e ctx]
          (when (:pressed? e)
            (cond
              (= :left (:key e))  (prev-turn!)
              (= :right (:key e)) (next-turn!)
              (and (= :r (:key e)) (:mac-command (:modifiers e))) (reset-game))))
        (ui/row
          [:stretch 1
           (ui/center
             (ui/padding 20 0 10 0
               (ui/with-bounds ::bounds
                 (ui/dynamic ctx [game      (current-game)
                                  tile-size (tile-size game (::bounds ctx))]
                   (ui/width (* tile-size (:width game))
                     (ui/height (* tile-size (:height game))
                       (ui/canvas {:on-paint on-paint})))))))]
          (ui/width 300
            (ui/padding 10 20 20 20
              (ui/column
                [:stretch 1
                 (ui/vscrollbar
            
                   (ui/column
                     (ui/with-context
                       {:hui.button/padding-left 10
                        :hui.button/padding-top 7
                        :hui.button/padding-right 10
                        :hui.button/padding-bottom 7}
                       (ui/row
                         (ui/valign 0.5
                           (ui/dynamic _ [turn (:turn (current-game))]
                             (ui/label (str "Turn: " turn))))
                         [:stretch 1 nil]
                         (ui/valign 0.5
                           (ui/button prev-turn! (ui/label "???")))
                         (ui/gap 5 0)
                         (ui/valign 0.5
                           (ui/button next-turn! (ui/label "???")))))
                     (ui/gap 0 10)
                
                     (ui/slider *turn)
                     (ui/gap 0 15)
              
                     (ui/label "Scrap:")
                     (ui/gap 0 10)
                     (ui/height 50
                       (ui/canvas {:on-paint paint-scrap}))
                     (ui/gap 0 15)
                     
                     (ui/label "Tiles:")
                     (ui/gap 0 10)
                     (ui/height 50
                       (ui/canvas {:on-paint paint-tiles}))
                     (ui/gap 0 15)

                     (ui/label "Blue:")
                     (ui/gap 0 10)
                     (ui/dynamic _ [scrap (-> (current-game) :scrap :blue)]
                       (ui/padding 10 0 0 10
                         (ui/label (str "scrap = " scrap))))
                     (ui/dynamic _ [tiles (-> (current-game) :tiles :blue)]
                       (ui/padding 10 0 0 10
                         (ui/label (str "tiles = " tiles))))
                     (ui/dynamic _ [moves (->> (current-moves)
                                            (filter #(= :blue (second %)))
                                            (sort-by priority-fn))]
                       (ui/column
                         (for [[cmd _ & args] moves]
                           (ui/padding 10 0 0 10
                             (ui/label (str cmd " " (str/join " " args)))))))
                     (ui/gap 0 15)
              
                     (ui/label "Red:")
                     (ui/gap 0 10)
                     (ui/dynamic _ [scrap (-> (current-game) :scrap :red)]
                       (ui/padding 10 0 0 10
                         (ui/label (str "scrap = " scrap))))
                     (ui/dynamic _ [tiles (-> (current-game) :tiles :red)]
                       (ui/padding 10 0 0 10
                         (ui/label (str "tiles = " tiles))))
                     (ui/dynamic _ [moves (->> (current-moves)
                                            (filter #(= :red (second %)))
                                            (sort-by priority-fn))]
                       (ui/column
                         (for [[cmd _ & args] moves]
                           (ui/padding 10 0 0 10
                             (ui/label (str cmd " " (str/join " " args)))))))
                     (ui/gap 0 15)))]
                (ui/button #(reset-game) (ui/label "New game"))
                ))))))))

(reset! *app app)

(defn -main [& args]
  (alter-var-root #'clojure.pprint/*print-right-margin* (constantly 160))
  (alter-var-root #'clojure.main/report-error (constantly (fn [t _] (io.github.humbleui.error/log-error t))))
  (reset-game)
  (ui/start-app!
    (let [{screen :id
           bounds :bounds
           area   :work-area} (last (app/screens))]
      (reset! *window
        (ui/window
          {:title    "Coding Games Fall Challenge 2022"
           :mac-icon "resources/icon.icns"
           :screen   screen
           :x        (- (:x area) (:x bounds))
           :y        (- (:y area) (:y bounds))
           :width    (:width area)
           :height   (:height area)}
          *app))))
  (apply nrepl/-main args))

(comment
  (reset-game)
  @*games
  @*turn
  (-main))
