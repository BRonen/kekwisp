(ns bronen.cljisp-test
  (:require [clojure.test :refer [deftest testing is]]
            [bronen.cljisp :refer [lexer parse-expression, evaluate]]))

(deftest lexer-test
  (testing "TODO: implement lexer"
    (is (= (lexer "") nil))))

(deftest parser-test
  (testing "Should parse syntax tokens into a valid syntax tree"
    (is (= (parse-expression [{:token "literal" :value "println"}])
           {:token "literal" :value "println"}))
    (is (= (parse-expression [{:token "number" :value "5"}])
           {:token "number" :value 5}))
    (is (= (parse-expression [{:token "string" :value "lorem ipsum"}])
           {:token "string" :value "lorem ipsum"}))
    (is (= (parse-expression [{:token "lbraces"}
                              {:token "literal" :value "println"}
                              {:token "number" :value "5"}
                              {:token "string" :value "lorem ipsum"}
                              {:token "rbraces"}])
           {:token "list"
            :value [{:token "literal" :value "println"}
                    {:token "number" :value 5}
                    {:token "string" :value "lorem ipsum"}]}))))

(deftest eval-test
  (testing "Should evaluate a syntax tree and return a value"
    (is (= (evaluate {:token "literal" :value "example"} (atom {"example" 123})) 123))
    (is (= (evaluate {:token "number" :value 5} (atom {})) 5))
    (is (= (evaluate {:token "string" :value "lorem ipsum"} (atom {})) "lorem ipsum")))
  (testing "Should evaluate a syntax tree and mutate the original context"
    (let [ctx (atom {"example" 123})]
      (evaluate
       {:token "list"
        :value [{:token "literal" :value "def"}
                {:token "literal" :value "wasd"}
                {:token "number" :value 333}]}
       ctx)
      (is (= @ctx {"example" 123 "wasd" 333})))))

;(is (= (parse-expression [{:token "lbraces"}
;                              {:token "string" :value "lorem ipsum"}
;                              {:token "lbraces"}
;                              {:token "lbraces"}
;                              {:token "literal" :value "println"}
;                              {:token "number" :value "123"}
;                              {:token "rbraces"}
;                              {:token "number" :value "234"}
;                              {:token "rbraces"}
;                              {:token "rbraces"}])
;           {:token "list"
;            :value [{:token "string" :value "lorem ipsum"}
;                    {:token "list"
;                     :value [{:token "list"
;                              :value [{:token "literal" :value "println"}
;                                      {:token "number" :value 123}]}]}
;                    {:token "number" :value 234}]}))
