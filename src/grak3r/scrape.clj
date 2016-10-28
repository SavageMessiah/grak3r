(ns grak3r.scrape
  (:require [grak3r.graker :as grake]
            [grak3r.words :as words]
            [jsoup.soup :as soup])
  (:import [com.google.common.cache CacheBuilder CacheLoader]
           [java.util.concurrent TimeUnit]))

(defn fetch-url [url]
  (soup/get! url))

(defn scrape [fetch-fn {:keys [url selector] :as rule}]
  (let [page (fetch-fn url)
        words (soup/text (soup/select selector page))]
    (words/find-words words rule)))

(defrecord Scraper [cache fetch-fn]
  grake/Module
  (handle-grake [self env {:keys [type] :as rule}]
    (when (and (map? rule)
               (= :scrape type))
      (.get cache rule))))

(defn make-scraper
  ([] (make-scraper fetch-url))
  ([fetch-fn]
   (let [cache (-> (CacheBuilder/newBuilder)
                   (.maximumSize 100)
                   (.expireAfterWrite 1 TimeUnit/HOURS)
                   (.build (proxy [CacheLoader] []
                             (load [rule]
                               (scrape fetch-fn rule)))))]
     (->Scraper cache fetch-fn))))

