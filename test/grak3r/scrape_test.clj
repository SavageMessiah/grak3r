(ns grak3r.scrape-test
  (:require [clojure.test :refer :all]
            [grak3r.graker :as graker]
            [grak3r.words :as words]
            [grak3r.scrape :as scrape]))

(def test-html "<html><div>fat</div><div>cat</div></html>")

(def test-txt "fat\ncat\ndog")

(deftest test-selector
  (let [module (scrape/make-scraper (constantly test-html))]
    (are [e r] (= e (graker/handle-grake module nil r))
      #{"fat" "cat"}
      {:type :scrape :url "blah.html" :selector "div"}

      #{"fat"}
      {:type :scrape :url "blah.html" :selector "div" :begins-with "f"})))

(deftest test-lines
  (let [module (scrape/make-scraper (constantly test-txt))]
    (are [e r] (= e (graker/handle-grake module nil r))
      #{"fat" "cat" "dog"}
      {:type :scrape :url "blah.html" :selector :lines}

      #{"fat" "cat"}
      {:type :scrape :url "blah.html" :selector :lines :matches #"a"})))
