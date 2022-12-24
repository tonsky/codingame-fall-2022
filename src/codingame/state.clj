(ns codingame.state
  (:require
    [io.github.humbleui.window :as window]))

(def *app
  (atom nil))

(def *window
  (atom nil))

(defn redraw []
  (some-> @*window window/request-frame)
  :redraw)

(def *games
  (atom nil))

(def *turn
  (atom nil))