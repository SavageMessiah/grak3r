(ns grak3r.words
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [cheshire.core :as cheshire]
            [grak3r.graker :as grake])
  (:import [com.google.common.cache CacheBuilder CacheLoader]))

(defn my-keyword? [k tagged]
  (and (= "word" (namespace k))
       (get tagged (name k))))

(defn tagged-words [tagged tags]
  (apply set/intersection (vals (select-keys tagged tags))))

(defn find-words [words {:keys [matches begins-with ends-with]}]
  (if-not (or matches begins-with ends-with)
    words
    (into #{} (for [word words
                    :when (and (or (not matches)
                                   (re-find matches word))
                               (or (not begins-with)
                                   (str/starts-with? word begins-with))
                               (or (not ends-with)
                                   (str/ends-with? word ends-with)))]
                word))))

(defrecord Words [all tagged cache]
  grake/Module
  (handle-grake [self env rule]
    (cond (and (keyword? rule)
               (my-keyword? rule tagged))
          (grake/handle-grake self env {:type :word :tagged #{(name rule)}})
          (and (map? rule)
               (= (:type rule) :word))
          (.get cache rule)
          :else
          nil)))

(defn tag-word [tagged word tags]
  (reduce #(update %1 %2 (fnil conj #{}) word) tagged tags))

(defn add-word [words word-vec]
  (-> words
      (update :all conj (first word-vec))
      (update :tagged tag-word (first word-vec) (rest word-vec))))

(defn make-cache [{:keys [all tagged]}]
  (-> (CacheBuilder/newBuilder)
      (.maximumSize 100)
      (.build (proxy [CacheLoader] []
                (load [rule]
                  (find-words (if-let [tags (:tagged rule)]
                                (tagged-words tagged tags)
                                all)
                              rule))))))

(defn make-words
  ([]
   (make-words (-> "words.json"
                   clojure.java.io/resource
                   clojure.java.io/reader
                   cheshire/parse-stream)))
  ([word-vecs]
   (let [words (reduce add-word (map->Words {:all #{} :tagged {}})
                       word-vecs)]
     (assoc words :cache (make-cache words)))))
