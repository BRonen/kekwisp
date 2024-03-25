(ns bronen.cljisp-test
  (:require [clojure.test :refer [deftest testing is]]
            [bronen.cljisp :refer [lexer parse-expression]]))

(deftest lexer-test
  (testing "TODO: implement lexer"
    (is (= (lexer "") nil))))

(deftest parser-test
  (testing "TODO: implement parser"
    (is (= (parse-expression [{:token "number" :value "5"}])
           {:token "number" :value 5}))
    (is (= (parse-expression [{:token "string" :value "lorem ipsum"}])
           {:token "string" :value "lorem ipsum"}))
    (is (= (parse-expression [{:token "lbraces"}
                              {:token "number" :value "5"}
                              {:token "string" :value "lorem ipsum"}
                              {:token "rbraces"}])
           {:token "list"
            :value [{:token "number" :value 5}
                    {:token "string" :value "lorem ipsum"}]}))))
