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
  (println token tokens)
  (parse-list token tokens))
