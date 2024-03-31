# bronen/kekwisp

This project aims to be a funny lisp.

## Usage

Run the project's tests:
```
clojure -T:build test
```

## Examples

There are literals (variables), strings, integers, booleans and list that will be executed as a function call by default.

```clj
(print "string" 123 true)
```

You can declare variables using the "def" clause, here we define "foo" as the string "bar".

```clj
(def foo "bar")
```

We can run multiple statements using the "do" clause.

```clj
(do (def foo "bar") (print foo))
```

The "fn" clause can declare callables, the second element of the fn will not be executed as code. These literals will be considered just binders to the values that will be received by the call.

```clj
((fn (a b) (+ a b)) 1 2)
```

This callable can be stored on a variable declaration to be called later.

```clj
(do (def example (fn (a b) (+ a b))) (print (example 1 2)))
```

The "if" clause will evaluate the third or the fourth elements based on the second element.

```clj
(if true (print "hello world") (print "nah"))
```

There are a bunch of examples of valid code on the [tests](./test/bronen/kekwisp_test.clj).
