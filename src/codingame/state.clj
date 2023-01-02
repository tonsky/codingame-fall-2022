(ns  ^{:clojure.tools.namespace.repl/load false}
  codingame.state
  (:require
    [clojure.pprint :as pprint]
    [io.github.humbleui.window :as window])
  (:import
    [java.io Writer]))

(def *app
  (atom nil))

(def *window
  (atom nil))

(defn redraw []
  (some-> @*window window/request-frame)
  :redraw)

(def *games
  (atom nil))

(def *moves
  (atom nil))

(def *turn
  (atom nil))

;; state

(deftype Pos [^long x ^long y]
  java.lang.Object
  (toString [_]
    (str "(" x "," y ")"))
  
  (equals [a b]
    (identical? a b) true
    (not (instance? Pos b)) false
    (and (= x (:x b)) (= y (:y b))))
  
  (hashCode [_]
    (clojure.lang.Murmur3/hashInt (bit-or (bit-shift-left x 8) y)))
  
  clojure.lang.IHashEq
  (hasheq [_]
    (clojure.lang.Murmur3/hashInt (bit-or (bit-shift-left x 8) y)))
  
  clojure.lang.Indexed
  (nth [this i]
    (nth this i nil))
  (nth [_ i not-found]
    (case i
      0 x
      1 y
      not-found))
  
  clojure.lang.ILookup
  (valAt [_ k]
    (case k
      :x x
      :y y))
  (valAt [_ k not-found]
    (case k
      :x x
      :y y
      not-found))
  
  java.lang.Comparable
  (compareTo [a b]
    (cond
      (identical? a b)  0
      (< x (:x b))     -1
      (> x (:x b))      1
      (< y (:y b))     -1
      (> y (:y b))      1
      :else             0)))

(defn pos [x y]
  (Pos. x y))

(defmethod print-method Pos [c ^Writer w]
  (.write w "(")
  (.write w (str (:x c)))
  (.write w ",")
  (.write w (str (:y c)))
  (.write w ")"))

(defmethod pprint/simple-dispatch Pos [c]
  (.write ^Writer *out* "(")
  (.write ^Writer *out* (str (:x c)))
  (.write ^Writer *out* ",")
  (.write ^Writer *out* (str (:y c)))
  (.write ^Writer *out* ")"))

  
(defrecord Tile [^Pos pos owner ^long scrap ^long units ^long units-foe ^boolean recycler? ^boolean dead?]
  clojure.lang.Indexed
  (nth [this i]
    (nth this i nil))
  (nth [_ i not-found]
    (case i
      0 (.-x pos)
      1 (.-y pos)
      not-found)))

(defrecord Game [grid ^long width ^long height ^long turn scrap tiles])

(defprotocol Algo
  (-move [_ game]))

; [:move  <player> <units> <from> <to>]
; [:build <player> <pos>]
; [:spawn <player> <units> <pos>]