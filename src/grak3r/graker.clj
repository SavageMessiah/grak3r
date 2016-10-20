(ns grak3r.graker
  (:require [clojure.string :as str])
  (:import [java.util Random]))

(defprotocol Rule
  (grake [self env]))

(defn rand-int [{:keys [^Random rand]} limit]
  (.nextInt rand limit))

(defprotocol Module
  (handle-grake [self env rule]))

(defn handle-with-module [env rule]
  (if-let [result (some #(handle-grake % env rule) (:modules env))]
    (grake result env)
    (throw (ex-info "No module handled this" {:rule rule}))))

(extend-protocol Rule
  clojure.lang.IPersistentVector
  (grake [self env]
    (str/join " " (map #(grake % env) self)))
  java.lang.String
  (grake [self _] self)
  clojure.lang.Keyword
  (grake [self env]
    (handle-with-module env self))
  clojure.lang.IPersistentSet
  (grake [self env]
    (let [choice-vec (vec self)
          choice (get choice-vec (rand-int env (count choice-vec)))]
      (grake choice env)))
  clojure.lang.IPersistentMap
  (grake [self env]
    (handle-with-module env self))
  clojure.lang.IPersistentList
  (grake [self env]
    (if-let [builtin (get-in env [:builtins (first self)])]
      (grake (builtin env (rest self)) env)
      (throw (ex-info "Invalid builtin" {:builtin (first self)
                                         :params (rest self)})))))
