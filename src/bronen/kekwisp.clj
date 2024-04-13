(ns bronen.kekwisp
  (:require [clojure.core.reducers :as r]))

(defn quote? [s] (and (not= s \")
                      (not= s \))
                      (not= s \space)))

; Lexer

(defn lexer-literal
  "Tokenize a literal that begins with a non-digit until finds whitespace and returns the rest"
  [chars]
  (let [[content rst] (split-with quote? chars)]
    [rst {:token :literal :value content}]))

(defn lexer-number
  "Tokenize a string that begins with a digit until finds non-digit and returns the rest"
  [chars]
  (let [[content rst] (split-with #(Character/isDigit %) chars)]
    [rst {:token :number :value content}]))

(defn lexer-string
  "Tokenize a string that begins with a double quote until finds double quote and returns the rest"
  [[_ & chars]]
  (let [[content rst] (split-with quote? chars)]
    [(drop 1 rst) {:token :string :value content}]))

(defn lexer
  ([chars]
   (lexer chars []))
  ([chars acc]
   (cond
     (= (count chars) 0) acc
     (= (first chars) \space) (recur (drop 1 chars) acc)
     (= (first chars) \") (let [[rst token] (lexer-string chars)]
                            (recur rst (conj acc token)))
     (Character/isDigit (first chars)) (let [[rst token] (lexer-number chars)]
                                         (recur rst (conj acc token)))
     (= (first chars) \() (recur (drop 1 chars) (conj acc {:token :lbraces}))
     (= (first chars) \)) (recur (drop 1 chars) (conj acc {:token :rbraces}))
     :else (let [[rst token] (lexer-literal chars)]
             (recur rst (conj acc token))))))

; Parser

(defn parse-literal
  [token tokens]
  [{:node :literal :value (apply str (:value token))} tokens])

(defn parse-bool
  [token tokens]
  (case (:value token)
    [\t \r \u \e] [{:node :boolean :value true} tokens]
    [\f \a \l \s \e] [{:node :boolean :value false} tokens]
    (parse-literal token tokens)))

(defn parse-number
  [token tokens]
  (if (= (:token token) :number)
    [{:node :number
      :value (Integer/parseInt (apply str (:value token)))}
     tokens]
    (parse-bool token tokens)))

(defn parse-string
  [token tokens]
  (if (= (:token token) :string)
    [{:node :string
      :value (apply str (:value token))}
     tokens]
    (parse-number token tokens)))

(declare parse)

(defn parse-list
  [token tokens]
  (if (= (:token token) :lbraces)
    (loop [result []
           remaining tokens]
      (if (and (not= (count remaining) 0)
               (not= (-> remaining first :token) :rbraces))
        (let [[r rr] (parse remaining)] (recur (conj result r) rr))
        (let [rest-without-rbraces (drop 1 remaining)
              rest-or-nil (if (empty? rest-without-rbraces)
                            nil
                            rest-without-rbraces)]
          [{:node :list :value result} rest-or-nil])))
    (parse-string token tokens)))

(defn parse
  "Parses tokens to syntax tree"
  [[token & tokens]]
  (parse-list token tokens))

; Evaluation

(def default-context
  "A default map to look for default values"
  {"list" {:node :list}
   "map" {:node :map}
   "get" {:node :get}
   "fold" {:node :fold}
   "print" {:node :print}
   "def" {:node :definition}
   "fn" {:node :function}
   "do" {:node :do}
   "if" {:node :conditional}
   "+" {:node :sum}
   "-" {:node :subtraction}
   "*" {:node :multiplication}
   "/" {:node :division}})

(defn eval-literal
  [{_ :node value :value} ctx]
  (let [default-value (get default-context value)]
    (if (nil? default-value)
      (get @ctx value)
      default-value)))

(declare evaluate)

(defn eval-list
  [values ctx]
  (let [f (evaluate (first values) ctx)]
    (case (:node f)
      :callable ((:value f) (map #(evaluate % ctx)
                                 (drop 1 values)))
      :map (let [f (-> values (nth 1) (evaluate ctx) :value)]
             {:node :list
              :value (map #(f [%])
                          (-> values (nth 2) (evaluate ctx) :value))})
      :fold (let [f (-> values (nth 1) (evaluate ctx) :value)]
              (r/fold (fn
                        ([] (-> values (nth 2) (evaluate ctx)))
                        ([acc v] (f [acc v])))
                      (-> values (nth 3) (evaluate ctx) :value)))
      :get (let [i (-> values (nth 2) (evaluate ctx))]
             (-> values
                 (nth 1)
                 :value
                 (#(drop 1 %))
                 (nth i)
                 (evaluate ctx)))
      :list {:node :list
             :value (map #(evaluate % ctx)
                         (drop 1 values))}
      :sum (apply + (map #(evaluate % ctx)
                         (drop 1 values)))
      :subtraction (apply - (map #(evaluate % ctx)
                                 (drop 1 values)))
      :multiplication (apply * (map #(evaluate % ctx)
                                    (drop 1 values)))
      :division (apply / (map #(evaluate % ctx)
                              (drop 1 values)))
      :print (let [result (doall (map #(evaluate % ctx)
                                      (drop 1 values)))]
               (println result)
               result)
      :do (last (doall (map #(evaluate % ctx)
                            (drop 1 values))))
      :conditional (if (evaluate (nth values 1))
                     (evaluate (nth values 2))
                     (evaluate (nth values 3)))
      :definition (do (swap! ctx #(conj % {(:value (nth values 1))
                                           (evaluate (nth values 2))}))
                      (when (nth values 3 false) (evaluate (nth values 3) ctx)))
      :function {:node :callable
                 :value (fn [args]
                          (let [params (map :value (:value (nth values 1)))
                                arg-param-map (apply assoc {} (interleave params args))]
                            (swap! ctx #(conj % arg-param-map))
                            (evaluate (nth values 2) ctx)))})))

(defn evaluate
  "Evaluates a syntax tree"
  ([arg]
   (evaluate arg (atom {})))
  ([{node :node value :value} ctx]
   (case node
     :list (eval-list value ctx)
     :string value
     :number value
     :boolean value
     :literal (eval-literal {:node node :value value} ctx))))
