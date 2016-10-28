(ns grak3r.words-test
  (:require [clojure.test :refer :all]
            [grak3r.graker :as graker]
            [grak3r.words :as words]))

(def test-module (words/make-words [["cat" "noun" "animal"]
                                    ["hat" "noun"]
                                    ["cop" "noun"]
                                    ["crap" "adjective"]
                                    ["fat" "adjective"]
                                    ["kick" "verb"]]))

(def test-graker {:modules [test-module]
                  :builtins {}
                  :seed 1
                  :rand (java.util.Random. 1)})

(deftest test-words
  (are [r e] (= e (graker/handle-grake test-module test-graker r))
    :word/noun
    #{"cat" "hat" "cop"}

    :word/verb
    #{"kick"}

    {:type :word :tagged #{"noun" "animal"}}
    #{"cat"}

    {:type :word :ends-with "t"}
    #{"cat" "hat" "fat"}

    {:type :word :ends-with "t" :tagged #{"noun"}}
    #{"cat" "hat"}

    {:type :word :ends-with "at"}
    #{"cat" "hat" "fat"}

    {:type :word :begins-with "c"}
    #{"cat" "cop" "crap"}

    {:type :word :begins-with "c" :tagged #{"noun"}}
    #{"cat" "cop"}

    {:type :word :begins-with "kic"}
    #{"kick"}

    {:type :word :matches #"a"}
    #{"cat" "hat" "crap" "fat"}

    {:type :word :matches #"a" :tagged #{"noun"}}
    #{"cat" "hat"}

    {:type :word :matches #"a" :begins-with "h" :ends-with "t" :tagged #{"noun"}}
    #{"hat"}

    {:type :word}
    #{"cat" "hat" "cop" "crap" "fat" "kick"}

    {:type :word :ends-with "snurb"}
    #{}))
