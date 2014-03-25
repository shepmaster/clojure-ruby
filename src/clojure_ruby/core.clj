(ns clojure-ruby.core
  (:require [instaparse.core :as insta]))

(def ruby-parser
  (insta/parser (clojure.java.io/resource "ruby.ebnf")))

(def variables (atom {}))

(defn create-string [s]
  {:data s
   :methods {"size" (fn string-size [this] (count (:data this)))}})

(def Array
  {:methods {"new" (fn Array-new [this cnt val] (atom (vec (repeat cnt val))))}})

(swap! variables assoc "Array" Array)

(def File
  {:methods {"read" (fn File-read [this name] (create-string (slurp name)))}})

(swap! variables assoc "File" File)

(defmulti evaluate first)

(defmethod evaluate :method_call_infix [stmt]
  (let [[_ obj method arg] stmt
        arg (evaluate arg)]
    (assert (= :var_ref (first obj)))
    (case method
      "=" (swap! variables assoc (second obj) arg))))

(defn rb-send [obj method-name args]
  (if-let [meth (get-in obj [:methods method-name])]
    (apply meth obj args)
    (throw (RuntimeException. (str "Object " obj " does not have method " method-name)))))

(defmethod evaluate :var_ref [stmt]
  (let [[_ name] stmt]
    (if-let [var (get @variables name)]
      var
      (throw (RuntimeException. (str "Cannot find variable [" name "]"))))))

(defmethod evaluate :method_call_args [stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate obj)
        args (map evaluate args)]
    (rb-send obj method args)))

(defmethod evaluate :method_call_naked [stmt]
  ;; Duplication!
  (let [[_ obj method & args] stmt
        obj (evaluate obj)
        args (map evaluate args)]
    (rb-send obj method args)))

(defmethod evaluate :literal [stmt]
  (let [[_ val] stmt
        [type val] val]
    (case type
      :number (Long. val)
      :string val)))

(defmethod evaluate :while [stmt]
  (let [[_ predicate & body] stmt
        predicate (evaluate predicate)]
    (if predicate
      (map evaluate body))))

(defn evaluate-all [stmts]
  (map evaluate stmts))
