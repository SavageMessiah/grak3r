(ns grak3r.core
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [grak3r.graker :as graker]
            [grak3r.scrape :as scrape]
            [grak3r.words :as words]
            [grak3r.rules :as rules])
  (:import [java.util Random]))

(defn lift-fun [fun]
  (fn [env args]
    (apply fun (map #(graker/grake % env) args))))

(def default-builtins
  {'+ (fn [env args]
        (apply str (map #(graker/grake % env) args)))
   '- (fn [env [rule regex]]
        (let [w (graker/grake rule env)]
          (str/replace w regex "")))
   '? (fn [env args]
        (if (= 0 (graker/grand-int env 2))
          ""
          (graker/grake (first args) env)))
   'cap (lift-fun str/capitalize)
   'case/title (lift-fun str/capitalize)
   'case/up (lift-fun str/upper-case)
   'case/down (lift-fun str/lower-case)})

(def default-rules '{:cool-story ["Cool"
                                  (+ "br" (- {:type :word :tagged #{"adjective"} :matches #"^(o|\wo)"}
                                             #"^[^o]"))
                                  (+ "br" (- {:type :word :tagged #{"noun"} :matches #"^(o|\wo)"}
                                             #"^[^o]") ",")
                                  "Bro!"]
                     :insult/your-face ["Your face is like a" :word/adjective "butt that poops" (+ :word/noun "s") :word/adverb]
                     :bal-sagoth-team-name ["Team" #{["ScoutPrime"
                                                      (cap (+ :word/verb "ing"))
                                                      "Upon The"
                                                      (cap :word/noun)
                                                      "of"
                                                      (cap :word/adjective)
                                                      (cap :word/noun)]
                                                     ["A"
                                                      (cap :word/adjective)
                                                      (cap :word/noun)
                                                      (cap (+ :word/verb "s"))
                                                      "Over ScoutPrime"]
                                                     ["The"
                                                      (cap :word/noun)
                                                      "of a Thousand"
                                                      (cap (+ :word/noun "s"))
                                                      (cap (+ :word/verb "ing"))
                                                      "Beneath the Blazon of the"
                                                      (cap :word/adjective)
                                                      "ScoutPrime"]}]})

(defn new-graker
  ([] (new-graker (System/currentTimeMillis)))
  ([seed]
   {:modules [(scrape/make-scraper)
              (words/make-words)
              (rules/make-rules default-rules)]
    :builtins default-builtins
    :seed seed
    :rand (Random. seed)}))

(defn grake
  ([rule] (grake (new-graker) rule 10))
  ([rule n] (grake (new-graker) rule n))
  ([graker rule n]
   {:seed (:seed graker)
    :results (for [n (range 0 n)]
               (graker/grake rule graker))}))

(def ^:dynamic default-graker (new-graker))
(defn grake! [rule]
  (let [{:keys [seed results]} (grake default-graker rule 10)]
    (println "Seed: " seed)
    (doseq [res results]
      (println res))))
