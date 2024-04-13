(ns bronen.kekwisp-test
  (:require [clojure.test :refer [deftest testing is]]
            [bronen.kekwisp :refer [lexer parse evaluate]]))

(deftest lexer-test
  (testing "Should tokenize a string into valid tokens"
    (is (= (lexer "123")
           [{:token :number :value '(\1 \2 \3)}]))
    (is (= (lexer "\"lorem\"")
           [{:token :string :value '(\l \o \r \e \m)}]))
    (is (= (lexer "println")
           [{:token :literal :value '(\p \r \i \n \t \l \n)}]))
    (is (= (lexer "true")
           [{:token :literal :value '(\t \r \u \e)}]))
    (is (= (lexer "false")
           [{:token :literal :value '(\f \a \l \s \e)}]))
    (is (= (lexer "()")
           [{:token :lbraces} {:token :rbraces}]))
    (is (= (lexer ")(")
           [{:token :rbraces} {:token :lbraces}]))
    (is (= (lexer "123 \"lorem\"")
           [{:token :number :value '(\1 \2 \3)}
            {:token :string :value '(\l \o \r \e \m)}]))
    (is (= (lexer "println 123 \"lorem\"")
           [{:token :literal :value '(\p \r \i \n \t \l \n)}
            {:token :number :value '(\1 \2 \3)}
            {:token :string :value '(\l \o \r \e \m)}]))
    (is (= (lexer "(println 123 \"lorem\")")
           [{:token :lbraces}
            {:token :literal :value '(\p \r \i \n \t \l \n)}
            {:token :number :value '(\1 \2 \3)}
            {:token :string :value '(\l \o \r \e \m)}
            {:token :rbraces}]))))

(deftest parser-test
  (testing "Should parse syntax tokens into a valid syntax tree"
    (is (= (parse [{:token :literal :value '(\p \r \i \n \t \l \n)}])
           [{:node :literal :value "println"} nil]))
    (is (= (parse [{:token :number :value '(\5)}])
           [{:node :number :value 5} nil]))
    (is (= (parse [{:token :string :value '(\l \o \r \e \m \space \i \p \s \u \m)}])
           [{:node :string :value "lorem ipsum"} nil]))
    (is (= (parse [{:token :literal :value '(\t \r \u \e)}])
           [{:node :boolean :value true} nil]))
    (is (= (parse [{:token :literal :value '(\f \a \l \s \e)}])
           [{:node :boolean :value false} nil]))
    (is (= (parse [{:token :lbraces}
                   {:token :literal :value '(\p \r \i \n \t \l \n)}
                   {:token :number :value '(\5)}
                   {:token :string :value '(\l \o \r \e \m \space \i \p \s \u \m)}
                   {:token :rbraces}])
           [{:node :list
             :value [{:node :literal :value "println"}
                     {:node :number :value 5}
                     {:node :string :value "lorem ipsum"}]} nil]))
    (is (= (parse [{:token :lbraces}
                   {:token :string :value "lorem ipsum"}
                   {:token :lbraces}
                   {:token :lbraces}
                   {:token :literal :value "println"}
                   {:token :number :value "123"}
                   {:token :rbraces}
                   {:token :number :value "234"}
                   {:token :rbraces}
                   {:token :rbraces}])
           [{:node :list
             :value [{:node :string :value "lorem ipsum"}
                     {:node :list
                      :value [{:node :list
                               :value [{:node :literal :value "println"}
                                       {:node :number :value 123}]}
                              {:node :number :value 234}]}]} nil]))))

(deftest eval-test
  (testing "Should evaluate a syntax tree and return a value"
    (is (= (evaluate {:node :literal :value "example"} (atom {"example" 123})) 123))
    (is (= (evaluate {:node :number :value 5} (atom {})) 5))
    (is (= (evaluate {:node :string :value "lorem ipsum"} (atom {})) "lorem ipsum"))
    (is (= (evaluate {:node :boolean :value true} (atom {})) true))
    (is (= (evaluate {:node :boolean :value false} (atom {})) false)))
  (testing "Should evaluate a syntax tree and mutate the original context"
    (let [ctx (atom {"example" 123})]
      (evaluate
       {:node :list
        :value [{:node :literal :value "def"}
                {:node :literal :value "wasd"}
                {:node :number :value 333}]}
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
    (let [ctx (atom {})]
      (is (= (-> "true"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             true)))
    (let [ctx (atom {})]
      (is (= (-> "false"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             false)))
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
      (is (= (:node func) :callable))
      (is (= ((:value func) '(2 3)) 5))
      (is (= ((:value func) '(2 5)) 7))))
  (testing "Should evaluate a string that defines a function and and execute immediately"
    (let [ctx (atom {})]
      (is (= (-> "((fn (a b) (+ a b)) 12 233)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             245))))
  (testing "Should evaluate multiple expressions"
    (let [ctx (atom {})]
      (is (= (-> "(do (def addtwo (fn (a b) (+ a b))) (addtwo 1 2) (addtwo 23 44))"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             67)))
    (let [ctx (atom {})]
      (is (= (-> "(if true 123 444)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             123)))
    (let [ctx (atom {})]
      (is (= (-> "(if false 123 444)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             444))))
  (testing "Should evaluate lists calls"
    (let [ctx (atom {})]
      (is (= (-> "(list 1 2 3)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             {:node :list :value [1 2 3]})))
    (let [ctx (atom {})]
      (is (= (-> "(list (list 1 2 3) 4 5 6)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             {:node :list :value [{:node :list :value [1 2 3]} 4 5 6]}))))
  (testing "Should evaluate lists calls"
    (let [ctx (atom {})]
      (is (= (-> "(list 1 2 3)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             {:node :list :value [1 2 3]})))
    (let [ctx (atom {})]
      (is (= (-> "(list (list 1 2 3) 4 5 6)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             {:node :list :value [{:node :list :value [1 2 3]} 4 5 6]}))))
  (testing "Should evaluate map calls"
    (let [ctx (atom {})]
      (is (= (-> "(map (fn (a) 8) (list 1 2 3))"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             {:node :list :value [8 8 8]}))
      (is (= (-> "(map (fn (a) (* a 2)) (list 1 2 3))"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             {:node :list :value [2 4 6]}))))
  (testing "Should evaluate fold calls"
    (let [ctx (atom {})]
      (is (= (-> "(fold (fn (acc v) (+ acc v)) 0 (list 1 2 3))"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             6)))
    (let [ctx (atom {})]
      (is (= (-> "(fold (fn (acc v) (+ acc v)) 0 (map (fn (v) (* v 2)) (list 1 2 3)))"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             12))))
  (testing "Should evaluate get calls"
    (let [ctx (atom {})]
      (is (= (-> "(get (list 1 2 3) 2)"
                 (lexer)
                 (parse)
                 (first)
                 (#(evaluate % ctx)))
             3)))))
