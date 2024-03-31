(ns bronen.kekwisp)

(defn quote? [s] (and (not= s \")
                      (not= s \))
                      (not= s \space)))

; Lexer

(defn lexer-literal
  "Tokenize a literal that begins with a non-digit until finds whitespace and returns the rest"
  [chars]
  (let [[content rst] (split-with quote? chars)]
    [rst {:token "literal" :value content}]))

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

; Parser

(defn parse-literal
  [token tokens]
  [(conj token {:value (apply str (:value token))}) tokens])

(defn parse-bool
  [token tokens]
  (case (:value token)
    [\t \r \u \e] [{:token "boolean" :value true} tokens]
    [\f \a \l \s \e] [{:token "boolean" :value false} tokens]
    (parse-literal token tokens)))

(defn parse-number
  [token tokens]
  (if (= (:token token) "number")
    [(conj token {:value (Integer/parseInt (apply str (:value token)))}) tokens]
    (parse-bool token tokens)))

(defn parse-string
  [token tokens]
  (if (= (:token token) "string")
    [(conj token {:value (apply str (:value token))}) tokens]
    (parse-number token tokens)))

(declare parse)

(defn parse-list
  [token tokens]
  (if (= (:token token) "lbraces")
    (loop [result []
           remaining tokens]
      (if (and (not= (count remaining) 0)
               (not= (-> remaining first :token) "rbraces"))
        (let [[r rr] (parse remaining)] (recur (conj result r) rr))
        (let [rest-without-rbraces (drop 1 remaining)
              rest-or-nil (if (empty? rest-without-rbraces)
                            nil
                            rest-without-rbraces)]
          [{:token "list" :value result} rest-or-nil])))
    (parse-string token tokens)))

(defn parse
  "Parses tokens to syntax tree"
  [[token & tokens]]
  (parse-list token tokens))

; Evaluation

(def default-context
  {"print" (fn [v] {:token "print" :value v})
   "def" (fn [v] {:token "definition" :value v})
   "fn" (fn [v] {:token "function" :value v})
   "do" (fn [v] {:token "do" :value v})
   "if" (fn [v] {:token "conditional" :value v})
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
      "callable" ((:value f) (map #(evaluate % ctx) (drop 1 values)))
      "sum" (apply + (map #(evaluate % ctx) (drop 1 values)))
      "subtraction" (apply - (map #(evaluate % ctx) (drop 1 values)))
      "multiplication" (apply * (map #(evaluate % ctx) (drop 1 values)))
      "division" (apply / (map #(evaluate % ctx) (drop 1 values)))
      "print" (let [result (doall (map #(evaluate % ctx) (drop 1 values)))] (println result) result)
      "do" (last (doall (map #(evaluate % ctx) (drop 1 values))))
      "conditional" (if (evaluate (nth values 1)) (evaluate (nth values 2)) (evaluate (nth values 3)))
      "definition" (do (swap! ctx #(conj % {(:value (nth values 1))
                                            (evaluate (nth values 2))}))
                       (when (nth values 3 false) (evaluate (nth values 3) ctx)))
      "function" {:token "callable"
                  :value (fn [args]
                           (let [params (map :value (:value (nth values 1)))
                                 arg-param-map (apply assoc {} (interleave params args))]
                             (swap! ctx #(conj % arg-param-map))
                             (evaluate (nth values 2) ctx)))})))

(defn evaluate
  "Evaluates a syntax tree"
  ([arg]
   (evaluate arg (atom {})))
  ([{token :token value :value} ctx]
   (case token
     "list" (eval-list value ctx)
     "string" value
     "number" value
     "boolean" value
     "literal" (eval-literal {:token token :value value} ctx))))
