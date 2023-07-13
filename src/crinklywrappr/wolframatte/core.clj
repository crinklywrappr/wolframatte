(ns crinklywrappr.wolframatte.core
  (:require [clojure.string :as sg]
            [clojure.set :as st]
            [clojure.zip :as zip])
  (:import [com.wolfram.jlink MathLinkFactory]
           [com.wolfram.jlink Expr]
           [clojure.lang MapEntry]))

(def pow (keyword "^"))

(def arithmetic
  (atom
   {:+ "Plus"
    :- "Subtract"
    :* "Times"
    :/ "Divide"
    pow "Power"}))

(defn link [linkname]
  (doto
   (MathLinkFactory/createKernelLink
    (sg/join \space ["-linkmode" "launch"
                     "-linkname" (format "'%s'" linkname)]))
    (.discardAnswer)))

(defn answer [ml expression]
  (locking ml
    (doto ml
      (.putFunction "EvaluatePacket" 1)
      (.put expression)
      (.endPacket)
      (.waitForAnswer))
    (.getExpr ml)))

(defn bigdec? [x] (instance? BigDecimal x))
(defn bigint? [x] (instance? clojure.lang.BigInt x))
(defn biginteger? [x] (instance? BigInteger x))
(defn expression? [x] (instance? Expr x))
(defn shared-expr? [x]
  (or (string? x) (int? x) (double? x)
      (biginteger? x) (bigdec? x)))
(defn arithmetic? [x]
  (let [x (keyword (if (coll? x) (first x) x))]
    ((set (keys @arithmetic)) x)))
(defn symbol-expr? [x] (or (keyword? x) (symbol? x)))
(defn mapentry? [x] (instance? MapEntry x))

(defn expr-array [xs] (into-array Expr xs))

(defn expr
  "low-level expression constructor.  Useful for custom encoders.
  typ is one of :bigdec, :bigint, :complex, :int, :rational, :real, :string, :symbol.
  xs is a collection or sequence of expressions."
  ([x]
   (if (expression? x)
     x (Expr. x)))
  ([typ x]
   (Expr.
    (case typ
      :bigdec Expr/BIGDECIMAL
      :bigint Expr/BIGINTEGER
      :complex Expr/COMPLEX
      :int Expr/INTEGER
      :rational Expr/RATIONAL
      :real Expr/REAL
      :string Expr/STRING
      :symbol Expr/SYMBOL)
    x))
  ([typ x xs]
   (Expr. (if (some? typ)
            (expr typ x)
            (expr x))
          (expr-array xs))))

(declare encode)

(defn bigint->expr [x] (expr :bigint (str x)))
(defn arithmetic->expr [x]
  (if (coll? x)
    (expr :symbol (get @arithmetic (keyword (first x)))
          (mapv encode (rest x)))
    (expr :symbol (get @arithmetic (keyword x)))))
(defn symbol->expr [x] (expr :symbol (name x)))
(defn bool->expr [x] (expr :symbol (if x "True" "False")))
(defn coll->expr [xs] (expr :symbol "List" (mapv encode xs)))
(defn nil->expr [_] (expr :symbol "Null"))
(defn ratio->expr [x]
  (expr :symbol "Rational"
        [(expr (numerator x))
         (expr (denominator x))]))
(defn mapentry->expr [[k v]]
  (expr :symbol "Rule" [(encode k) (encode v)]))
(defn map->expr [xs]
  (expr :symbol "Association" (mapv mapentry->expr xs)))

(def encoders
  (atom
   (list
    [shared-expr? expr]
    [boolean? bool->expr]
    [bigint? bigint->expr]
    [ratio? ratio->expr]
    [arithmetic? arithmetic->expr]
    [symbol-expr? symbol->expr]
    [map? map->expr]
    [mapentry? mapentry->expr]
    [coll? coll->expr]
    [nil? nil->expr]
    [expression? identity])))

(defn add-encoder [pred xform]
  (swap! encoders conj [pred xform]))

(defn encode
  "high level expression encoder.  custom encodings can be provided by modifying encoders."
  ([x]
   (some
    (fn test-encoder* [[pred xform]]
      (when (pred x)
        (xform x)))
    @encoders))
  ([x xs]
   (expr nil (encode x) (mapv encode xs))))

(defn li [& xs]
  (coll->expr xs))

(defn call [f & args]
  (encode f args))

(defn prop [entity property]
  (encode entity [property]))

(declare decode)

(defn map-entry
  ([[k v]] (MapEntry/create k v))
  ([k v] (MapEntry/create k v)))

(def collection-transforms
  "maps collection types to a function that takes a seq
   and transforms that seq into the collection type"
  (atom
   {clojure.lang.MapEntry map-entry
    clojure.lang.PersistentVector vec
    clojure.lang.PersistentList (partial apply list)
    clojure.lang.PersistentArrayMap (partial into {})
    clojure.lang.PersistentHashMap (partial into {})}))

(defn add-collection-transform [type xform]
  (swap! collection-transforms assoc type xform))

(defn expression-types
  "returns the types associated w/ the given expression.
  useful for writing your own decoders.  you should also
  check `(.head expr)` and `(parts expr)`"
  [expr]
  (cond-> #{}
    (.atomQ expr) (conj :atom)
    (.bigDecimalQ expr) (conj :bigdec)
    (.bigIntegerQ expr) (conj :bigint)
    (.complexQ expr) (conj :complex)
    (.integerQ expr) (conj :int)
    (.listQ expr) (conj :list)
    (.matrixQ expr) (conj :matrix)
    (.numberQ expr) (conj :num)
    (.rationalQ expr) (conj :rational)
    (.realQ expr) (conj :real)
    (.stringQ expr) (conj :str)
    (.symbolQ expr) (conj :symbol)
    (.trueQ expr) (conj :true)
    (.vectorQ expr) (conj :vec)))

(defn parts [expression] (vec (.args expression)))

(defn expr-coll? [x] (.listQ x))

(defn expr-int? [x] (and (.integerQ x) (not (.bigIntegerQ x))))
(defn expr->int [x] (.asLong x))

(defn expr-bigint? [x] (.bigIntegerQ x))
(defn expr->bigint [x] (.asBigInteger x))

(defn expr-double? [x] (and (.realQ x) (not (.bigDecimalQ x))))
(defn expr->double [x] (.asDouble x))

(defn expr-bigdec? [x] (.bigDecimalQ x))
(defn expr->bigdec [x] (.asBigDecimal x))

(defn expr-string? [x] (.stringQ x))
(defn expr->string [x] (.asString x))

;; (encode 3/2) does not satisfy .rationalQ,
;; but route that expression through mathematica and it will
(defn expr-rational? [x] (= (str (.head x)) "Rational"))
(defn expr->rational [x]
  (let [[n d] (parts x)]
    (/ (expr->bigint n)
       (expr->bigint d))))

(defn expr-boolean? [x] (#{"True" "False"} (str x)))
(defn expr->boolean [x] (= (str x) "True"))

(defn expr-nil? [x] (= (str x) "Null"))

(defn expr-rule? [x] (= (str (.head x)) "Rule"))
(defn expr->rule [x] (map-entry (parts x)))

(defn expr-association? [x] (= (str (.head x)) "Association"))
(defn expr->association [x]
  (into {} (mapv decode (parts x))))

(defn expr-arithmetic? [x]
  (and ((set (vals @arithmetic)) (str (.head x)))
       (seq (parts x))))
(defn expr->arithmetic [x]
  (let [f (symbol (get (st/map-invert @arithmetic) (str (.head x))))]
    (apply (partial list f) (parts x))))

(defn expr-symbol? [x] (and (.symbolQ x) (empty? (parts x))))
(defn expr->symbol [x] (symbol (str x)))

(def decoders
  (atom
   (list
    [expr-coll? parts]
    [expr-int? expr->int]
    [expr-double? expr->double]
    [expr-bigint? expr->bigint]
    [expr-bigdec? expr->bigdec]
    [expr-rational? expr->rational]
    [expr-arithmetic? expr->arithmetic]
    [expr-symbol? expr->symbol]
    [expr-boolean? expr->boolean]
    [expr-string? expr->string]
    [expr-nil? (constantly nil)]
    [expr-rule? expr->rule]
    [expr-association? expr->association]
    [(constantly true) identity])))

(defn add-decoder [pred xform]
  (swap! decoders conj [pred xform]))

(defn make-node [node children]
  (try
    ((get @collection-transforms (type node)) children)
    (catch Exception ex
      (throw (ex-info "Unable to make node"
                      {:node node :children children :type (type node)} ex)))))

(defn expression-zip [expression]
  (zip/zipper coll? identity make-node expression))

(defn decode* [loc]
  (let [x (zip/node loc)]
    (if (expression? x)
      (some
       (fn test-decoder* [[pred xform]]
         (when (pred x)
           (zip/edit loc xform)))
       @decoders)
      loc)))

(defn decode [expression]
  (loop [z (expression-zip expression)]
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (decode* z))))))

(def ^:dynamic *mathlink* nil)

;; TODO: add docstring, improve params
(defn wrap-fn [nm]
  (fn [& args] (apply (partial call nm) args)))

(defmacro intern-fn [nm]
  `(def ~(symbol nm) (wrap-fn ~nm)))

(defmacro intern-fns [& names]
  (mapv (fn [x] `(intern-fn ~x)) names))

(defmacro with-link [link & forms]
  `(binding [*mathlink* ~link]
     ~@forms))

(defn math
  ([expression]
   (decode (answer *mathlink* expression)))
  ([expression & expressions]
   (mapv math (conj expressions expression))))
