(ns grak3r.scrape
  (:require [clojure.string :as str]
            [grak3r.graker :as grake]
            [grak3r.words :as words]
            [jsoup.soup :as soup])
  (:import [com.google.common.cache CacheBuilder CacheLoader LoadingCache]
           [java.util.concurrent TimeUnit]))

(defn fetch-url [url]
  (slurp url))

(defn scrape [fetch-fn {:keys [url selector] :as rule}]
  (let [page (fetch-fn url)
        words (if (= :lines selector)
                (set (str/split-lines page))
                (let [page (soup/parse (fetch-fn url))]
                  (set (soup/text (soup/select selector page)))))]
    (words/find-words words rule)))

(defrecord Scraper [^LoadingCache cache fetch-fn]
  grake/Module
  (handle-grake [self env {:keys [type url selector] :as rule}]
    (when (and (map? rule)
               (= :scrape type))
      (when (not (and url selector))
        (throw (ex-info "url and selector required" {:rule rule})))
      (when (not (string? url))
        (throw (ex-info "url must be string" {:rule rule})))
      (when (not (or (string? selector) (= :lines selector)))
        (throw (ex-info "selector must be string or :lines" {:rule rule})))
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

