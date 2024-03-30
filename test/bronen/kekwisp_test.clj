(ns bronen.kekwisp-test
  (:require [clojure.test :refer [deftest testing is]]
            [bronen.kekwisp :refer [lexer parse evaluate]]))

(deftest lexer-test
  (testing "Should tokenize a string into valid tokens"
    (is (= (lexer "123")
           [{:token "number" :value '(\1 \2 \3)}]))
    (is (= (lexer "\"lorem\"")
           [{:token "string" :value '(\l \o \r \e \m)}]))
    (is (= (lexer "println")
           [{:token "literal" :value '(\p \r \i \n \t \l \n)}]))
    (is (= (lexer "()")
           [{:token "lbraces"} {:token "rbraces"}]))
    (is (= (lexer ")(")
           [{:token "rbraces"} {:token "lbraces"}]))
    (is (= (lexer "123 \"lorem\"")
           [{:token "number" :value '(\1 \2 \3)}
            {:token "string" :value '(\l \o \r \e \m)}]))
    (is (= (lexer "println 123 \"lorem\"")
           [{:token "literal" :value '(\p \r \i \n \t \l \n)}
            {:token "number" :value '(\1 \2 \3)}
            {:token "string" :value '(\l \o \r \e \m)}]))
    (is (= (lexer "(println 123 \"lorem\")")
           [{:token "lbraces"}
            {:token "literal" :value '(\p \r \i \n \t \l \n)}
            {:token "number" :value '(\1 \2 \3)}
            {:token "string" :value '(\l \o \r \e \m)}
            {:token "rbraces"}]))))

(deftest parser-test
  (testing "Should parse syntax tokens into a valid syntax tree"
    (is (= (parse [{:token "literal" :value '(\p \r \i \n \t \l \n)}])
           [{:token "literal" :value "println"} nil]))
    (is (= (parse [{:token "number" :value '(\5)}])
           [{:token "number" :value 5} nil]))
    (is (= (parse [{:token "string" :value '(\l \o \r \e \m \space \i \p \s \u \m)}])
           [{:token "string" :value "lorem ipsum"} nil]))
    (is (= (parse [{:token "lbraces"}
                   {:token "literal" :value '(\p \r \i \n \t \l \n)}
                   {:token "number" :value '(\5)}
                   {:token "string" :value '(\l \o \r \e \m \space \i \p \s \u \m)}
                   {:token "rbraces"}])
           [{:token "list"
             :value [{:token "literal" :value "println"}
                     {:token "number" :value 5}
                     {:token "string" :value "lorem ipsum"}]} nil]))
    (is (= (parse [{:token "lbraces"}
                   {:token "string" :value "lorem ipsum"}
                   {:token "lbraces"}
                   {:token "lbraces"}
                   {:token "literal" :value "println"}
                   {:token "number" :value "123"}
                   {:token "rbraces"}
                   {:token "number" :value "234"}
                   {:token "rbraces"}
                   {:token "rbraces"}])
           [{:token "list"
             :value [{:token "string" :value "lorem ipsum"}
                     {:token "list"
                      :value [{:token "list"
                               :value [{:token "literal" :value "println"}
                                       {:token "number" :value 123}]}
                              {:token "number" :value 234}]}]} nil]))))

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

(deftest integration-test
  (testing "Should evaluate a string and return the result"
    (is (= (-> "example"
               (lexer)
               (parse)
               (first)
               (#(evaluate % (atom {"example" 123}))))
           123))
    (is (= (-> "(+ 1 2 3)"
               (lexer)
               (parse)
               (first)
               (evaluate))
           6)))
  (testing "Should evaluate a string, return the result and mutate the context"
    (let [ctx (atom {})]
      (is (= (-> "(def wasd 123 444)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             444))
      (is (= @ctx
             {"wasd" 123})))
    (let [ctx (atom {})]
      (is (= (-> "(def wasd 123 (def ddd 444))"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             nil))
      (is (= @ctx
             {"wasd" 123
              "ddd" 444}))))
  (testing "Should evaluate a string that defines a function"
    (let [ctx (atom {})
          func (-> "(fn (a b) (+ a b))"
                   (lexer)
                   (parse)
                   (first)
                   (#(evaluate % ctx)))]
      (is (= (:token func) "callable"))
      (is (= ((:value func) '(2 3)) 5))
      (is (= ((:value func) '(2 5)) 7))))
  (testing "Should evaluate a string that defines a function and and execute immediately"
    (let [ctx (atom {})]
      (is (= (-> "((fn (a b) (+ a b)) 12 233)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             245)))))
