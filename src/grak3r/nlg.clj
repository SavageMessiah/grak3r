(ns grak3r.nlg
  (:require [clojure.string :as str]
            [grak3r.graker :as grake])
  (:import [simplenlg.framework NLGFactory]
           [simplenlg.lexicon Lexicon]
           [simplenlg.realiser.english Realiser]
           [simplenlg.phrasespec SPhraseSpec]
           [simplenlg.features Feature Tense InterrogativeType]))

(defn keyword->enum-string [kw]
  (str/replace (str/upper-case (name kw)) "-" "_"))

(defn fix_ [s]
  (str/replace s "_" " "))

(defrecord NLG [^Lexicon lexicon ^NLGFactory factory ^Realiser realiser]
  grake/Module
  (handle-grake [self env {:keys [type subject verb object
                                  tense progressive interrogative
                                  phrase-only]}]
    (when (= type :nlg)
      (let [spec (.createClause factory)]
        (when subject
          (.setSubject spec (fix_ (grake/grake subject env))))
        (when object
          (.setObject spec (fix_ (grake/grake object env))))
        (.setVerb spec (fix_ (grake/grake verb env)))
        (.setFeature spec Feature/PROGRESSIVE (boolean progressive))
        (when interrogative
          (.setFeature spec Feature/INTERROGATIVE_TYPE
                       (InterrogativeType/valueOf (keyword->enum-string interrogative))))
        (when tense
          (.setFeature spec Feature/TENSE (Tense/valueOf (keyword->enum-string tense))))
        (.realiseSentence realiser spec)))))

(defn make-nlg []
  (let [lexicon (Lexicon/getDefaultLexicon)]
    (map->NLG {:lexicon lexicon
               :factory (NLGFactory. lexicon)
               :realiser (Realiser. lexicon)})))

