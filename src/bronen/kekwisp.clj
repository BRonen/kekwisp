(ns bronen.kekwisp)

(defn quote? [s] (not= s \"))

(defn lexer-literal
  "Tokenize a literal that begins with a non-digit until finds whitespace and returns the rest"
  [chars]
  (let [[content rst] (split-with #(not (= % \space)) chars)]
    [(drop 1 rst) {:token "literal" :value content}]))

(defn lexer-number
  "Tokenize a string that begins with a digit until finds non-digit and returns the rest"
  [chars]
  (let [[content rst] (split-with #(Character/isDigit %) chars)]
    [rst {:token "number" :value content}]))

(defn lexer-string
  "Tokenize a string that begins with a double quote until finds double quote and returns the rest"
  [[_ & chars]]
  (let [[content rst] (split-with quote? chars)]
    [(drop 1 rst) {:token "string" :value content}]))

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
     (= (first chars) \() (recur (drop 1 chars) (conj acc {:token "lbraces"}))
     (= (first chars) \)) (recur (drop 1 chars) (conj acc {:token "rbraces"}))
     :else (let [[rst token] (lexer-literal chars)]
             (recur rst (conj acc token))))))

(defn parse-literal
  [token _]
  (conj token {:value (apply str (:value token))}))

(defn parse-number
  [token tokens]
  (if (= (:token token) "number")
    (conj token {:value (Integer/parseInt (apply str (:value token)))})
    (parse-literal token tokens)))

(defn parse-string
  [token tokens]
  (if (= (:token token) "string")
    (conj token {:value (apply str (:value token))})
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
   "fn" (fn [v] {:token "function" :value v})
   "+" (fn [v] {:token "sum" :value v})
   "-" (fn [v] {:token "subtraction" :value v})
   "*" (fn [v] {:token "multiplication" :value v})
   "/" (fn [v] {:token "division" :value v})})

(defn eval-literal
  [{_ :token value :value} ctx]
  (let [default-value (get default-context value)]
    (if (nil? default-value)
      (get @ctx value)
      (default-value value))))

(declare evaluate)

(defn eval-list
  [values ctx]
  (let [f (evaluate (first values) ctx)]
    (case (:token f)
      "definition" (do (swap! ctx #(conj % {(:value (nth values 1))
                                            (:value (nth values 2))}))
                       (when (nth values 3 false) (evaluate (nth values 3) ctx)))
      "sum" (apply + (map #(evaluate % ctx) (drop 1 values)))
      "subtraction" (apply - (map #(evaluate % ctx) (drop 1 values)))
      "multiplication" (apply * (map #(evaluate % ctx) (drop 1 values)))
      "division" (apply / (map #(evaluate % ctx) (drop 1 values))) 
      "function" (swap! ctx #(conj % {(:value (get values 1))
                                      (:value (get values 2))})))))

(defn evaluate
  "Evaluates a syntax tree"
  ([arg]
   (evaluate arg (atom {})))
  ([{token :token value :value} ctx]
   (case token
     "list" (eval-list value ctx)
     "string" value
     "number" value
     "literal" (eval-literal {:token token :value value} ctx))))
