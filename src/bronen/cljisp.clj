(ns bronen.cljisp)

(defn lexer
  [chars]
  (println chars "not implemented yet"))

(defn parse-literal [token _] token)

(defn parse-number
  [token tokens]
  (if (= (:token token) "number")
    (conj token {:value (Integer/parseInt (:value token))})
    (parse-literal token tokens)))

(defn parse-string
  [token tokens]
  (if (= (:token token) "string")
    token
    (parse-number token tokens)))

(declare parse-expression)

(defn parse-list
  [token tokens]
  (if (= (:token token) "lbraces")
    (let [elems (take-while #(not= (:token %) "rbraces") tokens)]
      {:token "list" :value (map #(parse-expression [%]) elems)})
    (parse-string token tokens)))

(defn parse-expression
  "Parses tokens to syntax tree"
  [[token & tokens]]
  (parse-list token tokens))

(def default-context
  {"def" (fn [v] {:token "definition" :value v})
   "fn" (fn [v] {:token "function" :value v})})

(defn eval-literal
  [{token :token value :value} ctx]
  (when (= token "literal")
    (let [default-value (get default-context value)]
      (if (nil? default-value)
        (get @ctx value)
        (default-value value)))))

(defn eval-number
  [{token :token value :value} ctx]
  (if (= token "number")
    value
    (eval-literal {:token token :value value} ctx)))

(defn eval-string
  [{token :token value :value} ctx]
  (if (= token "string")
    value
    (eval-number {:token token :value value} ctx)))

(declare evaluate)

(defn eval-list
  [values ctx]
  (let [f (evaluate (first values) ctx)]
    (if (= (:token f) "definition")
      (swap! ctx #(conj % {(:value (get values 1))
                           (:value (get values 2))}))
      ;(if (= (:token f) "function")
        ;(swap! ctx #(conj % {(:value (get values 1))
                             ;(:value (get values 2))}))
      (println "error: calling a non callable"))));)

(defn evaluate
  "Evaluates a syntax tree"
  [{token :token value :value} ctx]
  (if (= token "list")
    (eval-list value ctx)
    (eval-string {:token token :value value} ctx)))
