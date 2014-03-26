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

(defn host-send [obj method-sym & args]
  (if-let [meth (get-in obj [:host-methods method-sym])]
    (apply meth obj args)
    (throw (RuntimeException. (str "object " obj " does not have host method " method-sym)))))

(defn rb-send [obj method-name args]
  (if-let [meth (get-in obj [:methods method-name])]
    (apply meth obj args)
    (throw (RuntimeException. (str "Object " obj " does not have method " method-name)))))

(defn create-number [n]
  {:data n
   :methods {"<" (fn number-lt [this other]
                   (let [other (host-send other :number)]
                     (< (:data this) other)))}
   :host-methods {:number (fn number-host-number [this] (:data this))}})

(defn create-string [s]
  {:data s
   :methods {"size" (fn string-size [this]
                      (create-number (count (:data this))))}
   :host-methods {:string (fn string-host-string [this] (:data this))}})

(def Array
  {:methods {"new" (fn Array-new [this cnt val]
                     (let [cnt (host-send cnt :number)
                           val (host-send val :number)]
                       (atom (vec (repeat cnt val)))))}})

(swap! variables assoc "Array" Array)

(def File
  {:methods {"read" (fn File-read [this name]
                      (let [name (host-send name :string)]
                        (create-string (slurp name))))}})

(swap! variables assoc "File" File)

(defmulti evaluate first)

(defmethod evaluate :assignment [stmt]
  (let [[_ name val] stmt
        val (evaluate val)]
    (swap! variables assoc name val)))

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
    (create-number (Long. val))))

(defmethod evaluate :string [stmt]
  (let [[_ val] stmt]
    (create-string val)))

(defmethod evaluate :while [stmt]
  (let [[_ predicate & body] stmt
        predicate (evaluate predicate)]
    (if predicate
      (map evaluate body))))

(defn evaluate-all [stmts]
  (when-let [[stmt & stmts] stmts]
    (try
      (prn stmt)
      (evaluate stmt)
      (catch Exception e
        (let [[start end] (insta/span stmt)]
          (throw (RuntimeException. (str "evaluation failed; [" start ", " end "]") e)))))
    (recur stmts)))
