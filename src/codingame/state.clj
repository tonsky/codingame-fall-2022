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

(defrecord Pos [^long x ^long y]
  java.lang.Object
  (toString [_]
    (str "(" x "," y ")"))
  
  clojure.lang.Indexed
  (nth [this i]
    (nth this i nil))
  (nth [_ i not-found]
    (case i 0 x 1 y not-found))
  
  java.lang.Comparable
  (compareTo [a b]
    (cond
      (identical? a b)  0
      (< (:x a) (:x b)) -1
      (> (:x a) (:x b)) 1
      (< (:y a) (:y b)) -1
      (> (:y a) (:y b)) 1
      :else             0)))

(def pos ->Pos)

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

(defrecord Tile [pos owner ^long scrap ^long units ^long units-foe ^boolean recycler?])

(defrecord Game [grid ^long width ^long height ^long turn scrap tiles])

(defprotocol Algo
  (-move [_ game]))

; [:move  <player> <units> <from> <to>]
; [:build <player> <pos>]
; [:spawn <player> <units> <pos>]