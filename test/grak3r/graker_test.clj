(ns grak3r.graker-test
  (:require[clojure.test :refer :all]
           [grak3r.graker :as g]))

(def test-graker (g/new-graker {:modules [(reify g/Module
                                            (handle-grake [_ _ _] "test-module"))]
                                :builtins {'test (fn [_ _] "test-builtin")}
                                :seed 1}))

(deftest test-basics
  (are [g e] (= e (g/grake g test-graker))
    "test" "test"
    "" ""
    #{"test"} "test"
    #{} ""
    #{"test1" "test2"} "test1"
    '(test "blah") "test-builtin"
    :test "test-module"
    {} "test-module"
    ["cats" "horses"] "cats horses"
    [] ""
    ["cats"] "cats"
    ["cats" "," "horses"] "cats, horses"
    ["cats" "" "horses"] "cats horses"))

