# crinklywrappr/wolframatte

This is a small, zero-dependency library that allows you to call mathematica.

## Coordinates

```
com.github.crinklywrappr/wolframatte {:mvn/version "1.0.6"}
```

## Why?

`clojuratica` isn't actively maintained and has been forked lots of times. `wolframite` is the successor, but it's unfinished and based on `clojuratica` like everything else. I wanted a simpler, fresh approach.

## Usage

### Connecting to mathematica

```clojure
(require '[crinklywrappr.wolframatte.core :refer :all])
(def ml (link "/path/to/jlink.jar"))
```

### Encoding expression

Mathematica/Wolfram One/Wolfram Player are all distributed with `JLink.jar`.  Be sure to include it in your project.

Use the following functions for encoding mathematica expressions.

```clojure
(li 1 2 3)
;=> Expr "{1, 2, 3}"

(call :Plus 1 2)
;=> Expr "Plus[1, 2]"

(prop {:a 1 :b 2} :a)
;=> Expr "Association[Rule[a, 1], Rule[b, 2]][1]"

;; general purpose encode
(encode {:a 1 :b 2})
;=> Expr "Association[Rule[a, 1], Rule[b, 2]]"
```

Nest expressions.
```clojure
(call :Divide (call :Plus 'a 1) (call :Plus 'b 1))
;=> Expr "Divide[Plus[a, 1], Plus[b, 1]]"
```

The arithmetic operations have special handling

```clojure
(li '(+ 1 2) '(- 1 2) '(* 1 2) '(/ 1 2) (list pow 1 2))
;=> Expr "{Plus[1, 2], Subtract[1, 2], Times[1, 2], Divide[1, 2], Power[1, 2]}"
```

### Decoding expressions 

Use `decode`:

```clojure
((juxt #(mapv type %) identity)
 (decode
  (li
   []
   1
   (BigInteger. "1")
   0.01
   (BigDecimal. "0.01")
   3/2
   true false
   "True" "False" "hello, world"
   nil
   {:a 1 :b 2} (first {:a 1 :b 2})
   '(+ 1 2) '(- 1 2) '(* 1 2) '(/ 1 2) (list pow 1 2))))
;=>
[[clojure.lang.PersistentVector
  java.lang.Long
  java.math.BigInteger
  java.lang.Double
  java.math.BigDecimal
  clojure.lang.Ratio
  clojure.lang.Symbol
  clojure.lang.Symbol
  java.lang.String
  java.lang.String
  java.lang.String
  clojure.lang.Symbol
  clojure.lang.PersistentArrayMap
  clojure.lang.MapEntry
  clojure.lang.PersistentList
  clojure.lang.PersistentList
  clojure.lang.PersistentList
  clojure.lang.PersistentList
  clojure.lang.PersistentList]
 [[]
  1
  1
  0.01
  0.01M
  3/2
  True
  False
  "True"
  "False"
  "hello, world"
  Null
  {a 1, b 2}
  [a 1]
  (+ 1 2)
  (- 1 2)
  (* 1 2)
  (/ 1 2)
  (^ 1 2)]]
```

### Querying mathematica

Use `answer`

```clojure
(decode (answer ml (call :FactorInteger 12345)))
;=> [[3 1] [5 1] [823 1]]
```

### Less boilerplate

I added some simple boilerplate reduction w/ the following functions and macros:
- `wrap-fn`: wrap a mathematica expression in a clojure function
- `intern-fns`: intern mathematica functions in your namespace by name
- `with-link`: provide the mathlink object for all encapsulated queries
- `math`: remove `(decode (answer ...))` boilerplate

```clojure
(intern-fns :Solve :Equal :Plus :Power :Times :Function :Take :Sort :Map :GenomeData)

(def genome-fn
    (wrap-fn
     (Function ['n]
               (Take
                (Sort
                 (Map
                  (Function ['gene]
                            [(GenomeData 'gene "SequenceLength") 'gene])
                  (GenomeData)))
                'n))))

 (with-link ml
    (math
     (genome-fn 4)
     (Solve (Equal (Plus (Power 'x 2) (Times 'a 'x) 1) 0) 'x)))               
;=>
[[[11 "IGHD727"] [16 "IGHD411"] [16 "IGHD417"] [16 "IGHD44"]]
 [[[x (* 1/2 (+ (* -1 a) (* -1 (^ (+ -4 (^ a 2)) 1/2))))]]
  [[x (* 1/2 (+ (* -1 a) (^ (+ -4 (^ a 2)) 1/2)))]]]]
```

## TODO

- Add specific instructions for different systems and build tools
- Add example repository

## License

Copyright © 2023 crinklywrappr

_EPLv1.0 is just the default for projects generated by `deps-new`: you are not_
_required to open source this project, nor are you required to use EPLv1.0!_
_Feel free to remove or change the `LICENSE` file and remove or update this_
_section of the `README.md` file!_

Distributed under the Eclipse Public License version 1.0.
