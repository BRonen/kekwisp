(ns bronen.cljisp)

(defn lexer
  [chars]
  (print chars "not implemented yet"))

(defn create-syntax-node
  [name & args]
  {:name name})

(defn parser
  "Parses tokens to syntax tree"
  [[token & tokens]]
  (println token tokens)
  (create-syntax-node token))
