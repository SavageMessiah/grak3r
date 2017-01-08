(ns grak3r.nlg
  (:require [clojure.string :as str]
            [grak3r.graker :as grake])
  (:import [simplenlg.framework PhraseElement NLGElement NLGFactory]
           [simplenlg.lexicon Lexicon]
           [simplenlg.realiser.english Realiser]
           [simplenlg.phrasespec SPhraseSpec]
           [simplenlg.features Feature NumberAgreement
            Tense InterrogativeType]))

(defrecord NLGPhrase [^Realiser realiser ^NLGElement element]
  grake/Rule
  (grake [_ _]
    (.toString (.realise realiser element))))

(defn keyword->enum-string [kw]
  (str/replace (str/upper-case (name kw)) "-" "_"))

(defn fix_ [s]
  (str/replace s "_" " "))



(defn get-element [env val]
  (let [val (grake/grake val env)]
    (if (isa? (type val) NLGPhrase)
      (:element val)
      (fix_ val))))

(defn sequentialize [v]
  (if-not (sequential? v)
    [v]
    v))

(defn expand-verb-rule [rule core-kw]
  (let [[_ will root end] (re-matches #"(will-)?(\w+?)(ed|ing)?" (name core-kw))
        rule (-> rule
                 (dissoc core-kw)
                 (assoc :nlg/verb (get rule core-kw)
                        :type :nlg/verb))]
    (cond-> rule
      will
      (assoc :tense :future)
      (= "ing" end)
      (assoc :progressive true)
      (= "ed" end)
      (assoc :tense :past))))

(defn expand-noun-rule [rule core-kw]
  (let [[_ spec root s] (re-matches #"(a|the)?-?(\w+?)(s)?" (name core-kw))
        rule (-> rule
                 (dissoc core-kw)
                 (assoc :nlg/noun (get rule core-kw)
                        :type :nlg/noun))]
    (cond-> rule
      spec
      (assoc :specifier spec)
      s
      (assoc :plural true)
      (= "haver" root)
      (assoc :possessive true))))

(defn expand-clause-rule [rule core-kw]
  (let [[_ will end ?] (re-matches #"(will-)?claus(e|ed|ing)(\?)?"
                                     (name core-kw))
        rule (-> rule
                 (dissoc core-kw)
                 (assoc :type :nlg/clause))]
    (cond-> rule
      will
      (assoc :tense :future)
      (= "ing" end)
      (assoc :progressive true)
      (= "ed" end)
      (assoc :tense :past)
      ?
      (assoc :interrogative :yes-no))))

(defn expand-rule [rule core-kw]
  (cond
    (#{:nlg/adjective :nlg/adverb} core-kw)
    (assoc rule :type core-kw)
    (re-matches #".*claus.*" (name core-kw))
    (expand-clause-rule rule core-kw)
    (re-matches #".*verb.*" (name core-kw))
    (expand-verb-rule rule core-kw)
    :else
    (expand-noun-rule rule core-kw)))

(defn get-core-kw [{:keys [type] :as rule}]
  (if (and type (= "nlg" (namespace type)))
    type
    (first (filter #(= "nlg" (namespace %))
          (keys rule)))))

(defn add-mods [^PhraseElement spec env pre post]
  (when pre
    (doseq [pre (sequentialize pre)]
      (.addPreModifier spec (get-element env pre))))
  (when post
    (doseq [post (sequentialize post)]
      (.addPostModifier spec (get-element env post)))))

(defmulti make-phrase (fn [nlg env rule] (:type rule)))

(defmethod make-phrase :nlg/verb
  [{:keys [^NLGFactory factory
           ^Realiser realiser]}
   env
   {:keys [:nlg/verb front pre post progressive tense]}]
  (let [spec (.createVerbPhrase factory)]
    (.setVerb spec (get-element env verb))
    (add-mods spec env pre post)
    (.setFeature spec Feature/PROGRESSIVE (boolean progressive))
    (when tense
      (.setFeature spec Feature/TENSE (Tense/valueOf (keyword->enum-string tense))))
    (->NLGPhrase realiser spec)))

(defmethod make-phrase :nlg/noun
  [{:keys [^NLGFactory factory
           ^Realiser realiser]}
   env
   {:keys [:nlg/noun front pre post plural
           possessive specifier]}]
  (let [spec (.createNounPhrase factory)]
    (.setNoun spec (get-element env noun))
    (add-mods spec env pre post)
    (when specifier
      (.setSpecifier spec (get-element env specifier)))
    (when plural
      (.setFeature spec Feature/NUMBER NumberAgreement/PLURAL))
    (when possessive
      (.setFeature spec Feature/POSSESSIVE (boolean possessive)))
    (->NLGPhrase realiser spec)))

(defmethod make-phrase :nlg/adjective
  [{:keys [^NLGFactory factory
           ^Realiser realiser]}
   env
   {:keys [:nlg/adjective pre]}]
  (let [spec (.createAdjectivePhrase factory)]
    (.setAdjective spec (get-element env adjective))
    (add-mods spec env pre nil)
    (->NLGPhrase realiser spec)))

(defmethod make-phrase :nlg/adverb
  [{:keys [^NLGFactory factory
           ^Realiser realiser]}
   env
   {:keys [:nlg/adverb pre]}]
  (let [spec (.createAdverbPhrase factory)]
    (.setAdverb spec (get-element env adverb))
    (add-mods spec env pre nil)
    (->NLGPhrase realiser spec)))

(defmethod make-phrase :nlg/clause
  [{:keys [^NLGFactory factory
           ^Realiser realiser]} env
   {:keys [subject verb object
           tense progressive interrogative]}]
  (let [spec (.createClause factory)]
    (when subject
      (.setSubject spec (get-element env subject)))
    (when object
      (.setObject spec (get-element env object)))
    (when verb
      (.setVerb spec (get-element env verb)))
    (.setFeature spec Feature/PROGRESSIVE (boolean progressive))
    (when interrogative
      (.setFeature spec Feature/INTERROGATIVE_TYPE
                   (InterrogativeType/valueOf (keyword->enum-string interrogative))))
    (when tense
      (.setFeature spec Feature/TENSE (Tense/valueOf (keyword->enum-string tense))))
    (->NLGPhrase realiser spec)))

(defrecord NLG [^Lexicon lexicon ^NLGFactory factory ^Realiser realiser]
  grake/Module
  (handle-grake [self env rule]
    (when (map? rule)
      (when-let [core-kw (get-core-kw rule)]
        (let [rule (expand-rule rule core-kw)]
          (make-phrase self env rule))))))

(defn make-nlg []
  (let [lexicon (Lexicon/getDefaultLexicon)]
    (map->NLG {:lexicon lexicon
               :factory (NLGFactory. lexicon)
               :realiser (Realiser. lexicon)})))
