  (ns ^{:clojure.tools.namespace.repl/load false}
  user
  (:require
    [clojure.tools.namespace.repl :as ns]))

(ns/set-refresh-dirs "src")

(defn reload []
  (set! *warn-on-reflection* true)
  (let [res (ns/refresh)]
    (if (instance? Throwable res)
      (throw res)
      res)))
