(ns clojure-ruby.core
  (:require [instaparse.core :as insta]))

(def ruby-parser
  (insta/parser (clojure.java.io/resource "ruby.ebnf")))

(defn rename-infix [obj meth arg]
  [:method_call obj meth arg])

(defn rename-bracket [obj idx]
  [:method_call obj "[]" idx])

(defn rename-bracket-assignment [obj idx val]
  [:method_call obj "[]=" idx])

(def cleaning-map
  {:method_call_infix rename-infix
   :method_call_bracket rename-bracket
   :method_call_bracket_assignment rename-bracket-assignment})

(defn clean-parse-tree [tree]
  (insta/transform cleaning-map tree))

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

(defmethod evaluate :assignment [stmt]
  (let [[_ name val] stmt
        val (evaluate val)]
    (swap! variables assoc name val)))

(defn rb-send [obj method-name args]
  (if-let [meth (get-in obj [:methods method-name])]
    (apply meth obj args)
    (throw (RuntimeException. (str "Object " obj " does not have method " method-name)))))

(defmethod evaluate :var_ref [stmt]
  (let [[_ name] stmt]
    (if-let [var (get @variables name)]
      var
      (throw (RuntimeException. (str "Cannot find variable [" name "]"))))))

(defmethod evaluate :method_call [stmt]
  (let [[_ obj method & args] stmt
        obj (evaluate obj)
        args (map evaluate args)]
    (rb-send obj method args)))

(defmethod evaluate :number [stmt]
  (let [[_ val] stmt]
    (Long. val)))

(defmethod evaluate :string [stmt]
  (let [[_ val] stmt]
    val))

(defmethod evaluate :while [stmt]
  (let [[_ predicate & body] stmt
        predicate (evaluate predicate)]
    (if predicate
      (map evaluate body))))

(defn evaluate-all [stmts]
  (map evaluate stmts))
