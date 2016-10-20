(ns grak3r.rules
  (:require [grak3r.graker :as grake]))


(defrecord Rules [rules]
  grake/Module
  (handle-grake [self env rule]
    (when (keyword? rule)
      (get rules rule))))

(defn make-rules [rules]
  (->Rules rules))

