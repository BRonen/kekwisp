(ns bronen.cljisp-test
  (:require [clojure.test :refer [deftest testing is]]
            [bronen.cljisp :refer [lexer parser]]))

(deftest lexer-test
  (testing "TODO: implement lexer"
    (is (= (lexer "") nil))))

(deftest parser-test
  (testing "TODO: implement parser"
    (is (= (parser ["number"]){:name "number"}))))
