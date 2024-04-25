(ns bronen.experimental.type-checker
  (:require [bronen.kekwisp :refer [lexer parse eval-literal]]))

;; Type check

(declare check)

(defn check-literal
  [expr ctx]
  (-> expr (eval-literal ctx) (check ctx)))

(defn check-list
  [{exprs :value} ctx]
  (let [called (-> exprs first (check ctx))
        params (drop 1 exprs)
        check-params (:params called)
        check-return (:return called)]
    (if (check-params params ctx)
      (check-return params ctx)
      {:node :error :values params})))

(def check-sum
  {:type :callable
   :params (fn [params ctx]
             (every? #(= :number (:type (check % ctx))) params))
   :return (fn [_ _] :number)})

(def check-function
  {:type :callable
   :params (fn [[args _] _] (= :list (:node args))) ;; set every arg as the type
   :return (fn [[_ body] ctx] (check body ctx))})

(defn check
  "Checks the type of an expression"
  ([expr]
   (check expr (atom {})))
  ([expr ctx]
   (if expr
     (case (:node expr)
       :string {:type :string}
       :number {:type :number}
       :boolean {:type :boolean}
       :literal (check-literal expr ctx)
       :list (check-list expr ctx)
       :sum check-sum
       :function check-function
       {:type :error :value expr})
     {:type :error :value :null-error})))


(comment
  (-> "(fn (a) 2)"
      lexer
      parse
      first
      check)

  nil)