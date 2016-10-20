(ns grak3r.words
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as cheshire]
            [grak3r.graker :as grake]))

(defn my-keyword? [k tagged]
  (and (= "word" (namespace k))
       (get tagged (name k))))

(defn tagged-words [tagged tags]
  (into #{} (for [tag tags]
              (get tagged tag))))

(defn find-words [words {:keys [matches begins-with ends-with]}]
  (if-not (and matches begins-with ends-with)
    words
    (into #{} (for [word words
                    :when (and (or (not matches)
                                   (re-find matches word))
                               (or (not begins-with)
                                   (str/starts-with? word begins-with))
                               (or (not ends-with)
                                   (str/ends-with? word ends-with)))]
                word))))

(defrecord Words [all tagged]
  grake/Module
  (handle-grake [self env rule]
    (cond (and (keyword? rule)
               (my-keyword? rule tagged))
          (grake/handle-grake self env {:type :word :tagged #{(name rule)}})
          (and (map? rule)
               (= (:type rule) :word))
          (find-words (if-let [tags (:tagged rule)]
                        (tagged-words tagged tags)
                        all)
                      rule)
          :else
          nil)))

(defn tag-word [tagged word tags]
  (reduce #(update %1 %2 (fnil conj #{}) word) tagged tags))

(defn add-word [words word-vec]
  (-> words
      (update :all conj (first word-vec))
      (update :tagged tag-word (first word-vec) (rest word-vec))))

(defn make-words []
  (let [word-vecs (-> "words.json"
                      clojure.java.io/resource
                      clojure.java.io/reader
                      cheshire/parse-stream)]
    (reduce add-word (map->Words {:all #{} :tagged {}}) word-vecs)))
