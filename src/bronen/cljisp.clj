(ns bronen.cljisp)

(defn lexer
  [chars]
  (println chars "not implemented yet"))

(defn parse-number
  [token]
  (println ">>>" token)
  (conj token {:value (Integer/parseInt (:value token))}))

(defn parser
  "Parses tokens to syntax tree"
  [[token & tokens]]
  (println token tokens)
  (if (= (:token token) "number")
    (parse-number token)
    ()))
